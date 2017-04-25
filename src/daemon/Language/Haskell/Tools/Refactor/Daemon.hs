{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           , MultiWayIf
           #-}
module Language.Haskell.Tools.Refactor.Daemon where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference
import qualified Data.Aeson as A ((.=))
import Data.Aeson hiding ((.=))
import Data.Algorithm.Diff
import qualified Data.ByteString.Char8 as StrictBS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
import Data.IORef
import Data.List hiding (insert)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import GHC.Generics
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy
import System.Directory
import System.Environment
import System.IO
import System.IO.Strict as StrictIO (hGetContents)
import Data.Version

import Bag
import DynFlags
import ErrUtils
import FastString (unpackFS)
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc, modifySession)
import HscTypes (hsc_mod_graph)
import Packages
import SrcLoc

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.Daemon.PackageDB
import Language.Haskell.Tools.Refactor.Daemon.State
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session
import Paths_haskell_tools_daemon

runDaemonCLI :: IO ()
runDaemonCLI = getArgs >>= runDaemon

runDaemon :: [String] -> IO ()
runDaemon args = withSocketsDo $
    do let finalArgs = args ++ drop (length args) defaultArgs
           isSilent = read (finalArgs !! 1)
       hSetBuffering stdout LineBuffering
       hSetBuffering stderr LineBuffering
       when (not isSilent) $ putStrLn $ "Starting Haskell Tools daemon"
       sock <- socket AF_INET Stream 0
       setSocketOption sock ReuseAddr 1
       when (not isSilent) $ putStrLn $ "Listening on port " ++ finalArgs !! 0
       bind sock (SockAddrInet (read (finalArgs !! 0)) iNADDR_ANY)
       listen sock 4
       clientLoop isSilent sock

defaultArgs :: [String]
defaultArgs = ["4123", "True"]

clientLoop :: Bool -> Socket -> IO ()
clientLoop isSilent sock
  = do when (not isSilent) $ putStrLn $ "Starting client loop"
       (conn,_) <- accept sock
       ghcSess <- initGhcSession
       state <- newMVar initSession
       serverLoop isSilent ghcSess state conn
       sessionData <- readMVar state
       when (not (sessionData ^. exiting))
         $ clientLoop isSilent sock

serverLoop :: Bool -> Session -> MVar DaemonSessionState -> Socket -> IO ()
serverLoop isSilent ghcSess state sock =
    do msg <- recv sock 2048
       when (not $ BS.null msg) $ do -- null on TCP means closed connection
         when (not isSilent) $ putStrLn $ "message received: " ++ show (unpack msg)
         let msgs = BS.split '\n' msg
         continue <- forM msgs $ \msg -> respondTo ghcSess state (sendAll sock . (`BS.snoc` '\n')) msg
         sessionData <- readMVar state
         when (not (sessionData ^. exiting) && all (== True) continue)
           $ serverLoop isSilent ghcSess state sock
  `catch` interrupted
  where interrupted = \ex -> do
                        let err = show (ex :: IOException)
                        when (not isSilent) $ do
                          putStrLn "Closing down socket"
                          hPutStrLn stderr $ "Some exception caught: " ++ err

respondTo :: Session -> MVar DaemonSessionState -> (ByteString -> IO ()) -> ByteString -> IO Bool
respondTo ghcSess state next mess
  | BS.null mess = return True
  | otherwise
  = case decode mess of
      Nothing -> do next $ encode $ ErrorMessage $ "MALFORMED MESSAGE: " ++ unpack mess
                    return True
      Just req -> modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient (next . encode) req) st) ghcSess)

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: (ResponseMsg -> IO ()) -> ClientMessage -> StateT DaemonSessionState Ghc Bool
updateClient resp (Handshake _) = liftIO (resp $ HandshakeResponse $ versionBranch version) >> return True
updateClient resp KeepAlive = liftIO (resp KeepAliveResponse) >> return True
updateClient resp Disconnect = liftIO (resp Disconnected) >> return False
updateClient _ (SetPackageDB pkgDB) = modify (packageDB .= pkgDB) >> return True
updateClient resp (AddPackages packagePathes) = do
    addPackages resp packagePathes
    return True
updateClient _ (RemovePackages packagePathes) = do
    mcs <- gets (^. refSessMCs)
    let existing = map ms_mod (mcs ^? traversal & filtered isRemoved & mcModules & traversal & modRecMS)
    lift $ forM_ existing (\modName -> removeTarget (TargetModule (GHC.moduleName modName)))
    lift $ deregisterDirs (mcs ^? traversal & filtered isRemoved & mcSourceDirs & traversal)
    modify $ refSessMCs .- filter (not . isRemoved)
    modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` existing) . ms_mod) (hsc_mod_graph s) })
    mcs <- gets (^. refSessMCs)
    when (null mcs) $ modify (packageDBSet .= False)
    return True
  where isRemoved mc = (mc ^. mcRoot) `elem` packagePathes

updateClient resp (ReLoad added changed removed) =
  -- TODO: check for changed cabal files and reload their packages
  do mcs <- gets (^. refSessMCs)
     removedMods <- gets (map ms_mod . filter ((`elem` removed) . getModSumOrig) . (^? refSessMCs & traversal & mcModules & traversal & modRecMS))
     lift $ forM_ removedMods (\modName -> removeTarget (TargetModule (GHC.moduleName modName)))
     -- remove targets deleted
     modify $ refSessMCs & traversal & mcModules
                .- Map.filter (\m -> maybe True (not . (`elem` removed) . getModSumOrig) (m ^? modRecMS))
     modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` removedMods) . ms_mod) (hsc_mod_graph s) })
     -- reload changed modules
     -- TODO: filter those that are in reloaded packages
     reloadRes <- reloadChangedModules (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)]))
                                       (\mss -> resp (LoadingModules (map getModSumOrig mss)))
                                       (\ms -> getModSumOrig ms `elem` changed)
     mcs <- gets (^. refSessMCs)
     let mcsToReload = filter (\mc -> any ((mc ^. mcRoot) `isPrefixOf`) added && isNothing (moduleCollectionPkgId (mc ^. mcId))) mcs
     addPackages resp (map (^. mcRoot) mcsToReload) -- reload packages containing added modules
     liftIO $ case reloadRes of Left errs -> resp (either ErrorMessage CompilationProblem (getProblems errs))
                                Right _ -> return ()
     return True

updateClient _ Stop = modify (exiting .= True) >> return False

updateClient resp (PerformRefactoring refact modPath selection args) = do
    (selectedMod, otherMods) <- getFileMods modPath
    case selectedMod of
      Just actualMod -> do
        case analyzeCommand refact (selection:args) of
           Right cmd -> do res <- lift $ performCommand cmd actualMod otherMods
                           case res of
                             Left err -> liftIO $ resp $ ErrorMessage err
                             Right diff -> do changedMods <- applyChanges diff
                                              liftIO $ resp $ ModulesChanged (map (either id (\(_,_,ch) -> ch)) changedMods)
                                              void $ reloadChanges (map ((^. sfkModuleName) . (\(key,_,_) -> key)) (rights changedMods))
           Left err -> liftIO $ resp $ ErrorMessage err
      Nothing -> liftIO $ resp $ ErrorMessage $ "The following file is not loaded to Haskell-tools: "
                                                   ++ modPath ++ ". Please add the containing package."
    return True

  where applyChanges changes = do
          forM changes $ \case
            ModuleCreated n m otherM -> do
              mcs <- gets (^. refSessMCs)
              Just (_, otherMR) <- gets (lookupModInSCs otherM . (^. refSessMCs))

              let Just otherMS = otherMR ^? modRecMS
                  Just mc = lookupModuleColl (otherM ^. sfkModuleName) mcs
              modify $ refSessMCs & traversal & filtered (\mc' -> (mc' ^. mcId) == (mc ^. mcId)) & mcModules
                         .- Map.insert (SourceFileKey NormalHs n) (ModuleNotLoaded False)
              otherSrcDir <- liftIO $ getSourceDir otherMS
              let loc = toFileName otherSrcDir n
              liftIO $ withBinaryFile loc WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle (prettyPrint m)
              lift $ addTarget (Target (TargetModule (GHC.mkModuleName n)) True Nothing)
              return $ Right (SourceFileKey NormalHs n, loc, RemoveAdded loc)
            ContentChanged (n,m) -> do
              Just (_, mr) <- gets (lookupModInSCs n . (^. refSessMCs))
              let Just ms = mr ^? modRecMS
              let newCont = prettyPrint m
                  file = getModSumOrig ms
              origCont <- liftIO $ withBinaryFile file ReadMode $ \handle -> do
                hSetEncoding handle utf8
                StrictIO.hGetContents handle
              let undo = createUndo 0 $ getGroupedDiff origCont newCont
              origCont <- liftIO $ withBinaryFile file WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle newCont
              return $ Right (n, file, UndoChanges file undo)
            ModuleRemoved mod -> do
              Just (_,m) <- gets (lookupModInSCs (SourceFileKey NormalHs mod) . (^. refSessMCs))
              let modName = GHC.moduleName $ fromJust $ fmap semanticsModule (m ^? typedRecModule) <|> fmap semanticsModule (m ^? renamedRecModule)
              ms <- getModSummary modName
              let file = getModSumOrig ms
              origCont <- liftIO (StrictBS.unpack <$> StrictBS.readFile file)
              lift $ removeTarget (TargetModule modName)
              modify $ (refSessMCs .- removeModule mod)
              liftIO $ removeFile file
              return $ Left $ RestoreRemoved file origCont

        reloadChanges changedMods
          = do reloadRes <- reloadChangedModules (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)]))
                                                 (\mss -> resp (LoadingModules (map getModSumOrig mss)))
                                                 (\ms -> modSumName ms `elem` changedMods)
               liftIO $ case reloadRes of Left errs -> resp (either ErrorMessage (ErrorMessage . ("The result of the refactoring contains errors: " ++) . show) (getProblems errs))
                                          Right _ -> return ()

addPackages :: (ResponseMsg -> IO ()) -> [FilePath] -> StateT DaemonSessionState Ghc ()
addPackages resp [] = return ()
addPackages resp packagePathes = do
  nonExisting <- filterM ((return . not) <=< liftIO . doesDirectoryExist) packagePathes
  if (not (null nonExisting))
    then liftIO $ resp $ ErrorMessage $ "The following packages are not found: " ++ concat (intersperse ", " nonExisting)
    else do
      -- clear existing removed packages
      existingMCs <- gets (^. refSessMCs)
      let existing = map ms_mod $ (existingMCs ^? traversal & filtered isTheAdded & mcModules & traversal & modRecMS)
      needToReload <- handleErrors $ (filter (\ms -> not $ ms_mod ms `elem` existing))
                                       <$> getReachableModules (\_ -> return ()) (\ms -> ms_mod ms `elem` existing)
      modify $ refSessMCs .- filter (not . isTheAdded) -- remove the added package from the database
      forM_ existing $ \mn -> removeTarget (TargetModule (GHC.moduleName mn))
      modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` existing) . ms_mod) (hsc_mod_graph s) })
      -- load new modules
      pkgDBok <- initializePackageDBIfNeeded
      if pkgDBok then do
        res <- loadPackagesFrom (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)]) >> return (getModSumOrig ms))
                                (resp . LoadingModules . map getModSumOrig) (\st fp -> maybeToList <$> detectAutogen fp (st ^. packageDB)) packagePathes
        case res of
          Right (modules, ignoredMods) -> do
            mapM_ (reloadModule (\_ -> return ())) (either (const []) id needToReload) -- don't report consequent reloads (not expected)
            liftIO $ when (not $ null ignoredMods)
                       $ resp $ ErrorMessage
                                  $ "The following modules are ignored: "
                                       ++ concat (intersperse ", " ignoredMods)
                                       ++ ". Multiple modules with the same qualified name are not supported."
          Left err -> liftIO $ resp $ either ErrorMessage CompilationProblem (getProblems err)
      else liftIO $ resp $ ErrorMessage $ "Attempted to load two packages with different package DB. "
                                            ++ "Stack, cabal-sandbox and normal packages cannot be combined"
  where isTheAdded mc = (mc ^. mcRoot) `elem` packagePathes
        initializePackageDBIfNeeded = do
          pkgDBAlreadySet <- gets (^. packageDBSet)
          pkgDB <- gets (^. packageDB)
          locs <- liftIO $ mapM (packageDBLoc pkgDB) packagePathes
          case locs of
            firstLoc:rest ->
              if | not (all (== firstLoc) rest)
                     -> return False
                 | pkgDBAlreadySet -> do
                     pkgDBLocs <- gets (^. packageDBLocs)
                     return (pkgDBLocs == firstLoc)
                 | otherwise -> do
                     usePackageDB firstLoc
                     modify ((packageDBSet .= True) . (packageDBLocs .= firstLoc))
                     return True
            [] -> return True


data UndoRefactor = RemoveAdded { undoRemovePath :: FilePath }
                  | RestoreRemoved { undoRestorePath :: FilePath
                                   , undoRestoreContents :: String
                                   }
                  | UndoChanges { undoChangedPath :: FilePath
                                , undoDiff :: FileDiff
                                }
  deriving (Show, Generic)

instance ToJSON UndoRefactor

type FileDiff = [(Int, Int, String)]

createUndo :: Eq a => Int -> [Diff [a]] -> [(Int, Int, [a])]
createUndo i (Both str _ : rest) = createUndo (i + length str) rest
createUndo i (First rem : Second add : rest)
  = (i, i + length add, rem) : createUndo (i + length add) rest
createUndo i (First rem : rest) = (i, i, rem) : createUndo i rest
createUndo i (Second add : rest)
  = (i, i + length add, []) : createUndo (i + length add) rest
createUndo _ [] = []

initGhcSession :: IO Session
initGhcSession = Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags >> getSession))

usePackageDB :: GhcMonad m => [FilePath] -> m ()
usePackageDB [] = return ()
usePackageDB pkgDbLocs
  = do dfs <- getSessionDynFlags
       dfs' <- liftIO $ fmap fst $ initPackages
                 $ dfs { extraPkgConfs = (map PkgConfFile pkgDbLocs ++) . extraPkgConfs dfs
                       , pkgDatabase = Nothing
                       }
       void $ setSessionDynFlags dfs'

getProblems :: RefactorException -> Either String [(SrcSpan, String)]
getProblems (SourceCodeProblem errs) = Right $ map (\err -> (errMsgSpan err, show err)) $ bagToList errs
getProblems other = Left $ displayException other

data ClientMessage
  = KeepAlive
  | Handshake { clientVersion :: [Int] }
  | SetPackageDB { pkgDB :: PackageDB }
  | AddPackages { addedPathes :: [FilePath] }
  | RemovePackages { removedPathes :: [FilePath] }
  | PerformRefactoring { refactoring :: String
                       , modulePath :: FilePath
                       , editorSelection :: String
                       , details :: [String]
                       }
  | Stop
  | Disconnect
  | ReLoad { addedModules :: [FilePath]
           , changedModules :: [FilePath]
           , removedModules :: [FilePath]
           }
  deriving (Show, Generic)

instance FromJSON ClientMessage

data ResponseMsg
  = KeepAliveResponse
  | HandshakeResponse { serverVersion :: [Int] }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMarkers :: [(SrcSpan, String)] }
  | ModulesChanged { undoChanges :: [UndoRefactor] }
  | LoadedModules { loadedModules :: [(FilePath, String)] }
  | LoadingModules { modulesToLoad :: [FilePath] }
  | Disconnected
  deriving (Show, Generic)

instance ToJSON ResponseMsg

instance ToJSON SrcSpan where
  toJSON (RealSrcSpan sp) = object [ "file" A..= unpackFS (srcSpanFile sp)
                                   , "startRow" A..= srcLocLine (realSrcSpanStart sp)
                                   , "startCol" A..= srcLocCol (realSrcSpanStart sp)
                                   , "endRow" A..= srcLocLine (realSrcSpanEnd sp)
                                   , "endCol" A..= srcLocCol (realSrcSpanEnd sp)
                                   ]
  toJSON _ = Null
