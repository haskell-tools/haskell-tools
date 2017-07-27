{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           , MultiWayIf
           , TypeApplications
           , TypeFamilies
           #-}
module Language.Haskell.Tools.Refactor.Daemon where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Concurrent.Chan
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
import System.IO.Error
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
import Language.Haskell.Tools.Refactor.Daemon.Mode
import Language.Haskell.Tools.Refactor.Daemon.Protocol
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session
import Paths_haskell_tools_daemon

runDaemonCLI :: IO ()
runDaemonCLI = do store <- newEmptyMVar
                  getArgs >>= runDaemon socketMode store

runDaemon' :: [String] -> IO ()
runDaemon' args = do store <- newEmptyMVar
                     runDaemon socketMode store args

runDaemon :: WorkingMode a -> MVar a -> [String] -> IO ()
runDaemon mode connStore args = withSocketsDo $
    do let finalArgs = args ++ drop (length args) defaultArgs
           isSilent = read (finalArgs !! 1)
       hSetBuffering stdout LineBuffering
       hSetBuffering stderr LineBuffering
       when (not isSilent) $ putStrLn $ "Starting Haskell Tools daemon"
       conn <- daemonConnect mode finalArgs
       putMVar connStore conn
       when (not isSilent) $ putStrLn $ "Listening on port " ++ finalArgs !! 0
       ghcSess <- initGhcSession
       state <- newMVar initSession
       serverLoop mode conn isSilent ghcSess state
       daemonDisconnect mode conn

defaultArgs :: [String]
defaultArgs = ["4123", "True"]

serverLoop :: WorkingMode a -> a -> Bool -> Session -> MVar DaemonSessionState -> IO ()
serverLoop mode conn isSilent ghcSess state =
  ( do msgs <- daemonReceive mode conn
       continue <- forM msgs $ \case Right req -> respondTo ghcSess state (daemonSend mode conn) req
                                     Left msg -> do daemonSend mode conn $ ErrorMessage $ "MALFORMED MESSAGE: " ++ msg
                                                    return True
       sessionData <- readMVar state
       when (not (sessionData ^. exiting) && all (== True) continue)
         $ serverLoop mode conn isSilent ghcSess state
  `catchIOError` handleIOError )
  `catch` (\e -> handleException e >> serverLoop mode conn isSilent ghcSess state)
  where handleIOError err = hPutStrLn stderr $ "IO Exception caught: " ++ show err
        handleException ex = do
          let err = show (ex :: SomeException)
          hPutStrLn stderr $ "Exception caught: " ++ err
          daemonSend mode conn $ ErrorMessage $ "Internal error: " ++ err

respondTo :: Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ()) -> ClientMessage -> IO Bool
respondTo ghcSess state next req
  = modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient next req) st) ghcSess)

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: (ResponseMsg -> IO ()) -> ClientMessage -> StateT DaemonSessionState Ghc Bool
updateClient resp (Handshake _) = liftIO (resp $ HandshakeResponse $ versionBranch version) >> return True
updateClient resp KeepAlive = liftIO (resp KeepAliveResponse) >> return True
updateClient resp Disconnect = liftIO (resp Disconnected) >> return False
updateClient _ (SetPackageDB pkgDB) = modify (packageDB .= pkgDB) >> return True
updateClient resp (AddPackages packagePathes) = do
    addPackages resp packagePathes
    return True
updateClient _ (SetWorkingDir fp) = liftIO (setCurrentDirectory fp) >> return True
updateClient resp (SetGHCFlags flags) = lift (useFlags flags) >>= liftIO . resp . UnusedFlags >> return True
updateClient _ (RemovePackages packagePathes) = do
    mcs <- gets (^. refSessMCs)
    let existingFiles = concatMap @[] (map (^. sfkFileName) . Map.keys) (mcs ^? traversal & filtered isRemoved & mcModules)
    lift $ forM_ existingFiles (\fs -> removeTarget (TargetFile fs Nothing))
    lift $ deregisterDirs (mcs ^? traversal & filtered isRemoved & mcSourceDirs & traversal)
    modify $ refSessMCs .- filter (not . isRemoved)
    modifySession (\s -> s { hsc_mod_graph = filter ((`notElem` existingFiles) . getModSumOrig) (hsc_mod_graph s) })
    mcs <- gets (^. refSessMCs)
    when (null mcs) $ modify (packageDBSet .= False)
    return True
  where isRemoved mc = (mc ^. mcRoot) `elem` packagePathes

updateClient resp (ReLoad added changed removed) =
  -- TODO: check for changed cabal files and reload their packages
  do mcs <- gets (^. refSessMCs)
     lift $ forM_ removed (\src -> removeTarget (TargetFile src Nothing))
     -- remove targets deleted
     modify $ refSessMCs & traversal & mcModules
                .- Map.filter (\m -> maybe True ((`notElem` removed) . getModSumOrig) (m ^? modRecMS))
     modifySession (\s -> s { hsc_mod_graph = filter (\mod -> getModSumOrig mod `notElem` removed) (hsc_mod_graph s) })
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

-- TODO: perform refactorings without selected modules
updateClient resp (PerformRefactoring refact modPath selection args) = do
    (selectedMod, otherMods) <- getFileMods modPath
    case (selectedMod, analyzeCommand refact (selection:args)) of
      (Just actualMod, Right cmd) -> performRefactoring cmd actualMod otherMods
      (Nothing, Right cmd)
        -> case modPath of
             "" -> case otherMods of -- empty module path is no module selected (example: ProjectOrganizeImports)
                     mod:rest -> performRefactoring cmd mod rest
                     [] -> return ()
             _ -> liftIO $ resp $ ErrorMessage $ "The following file is not loaded to Haskell-tools: "
                                                        ++ modPath ++ ". Please add the containing package."
      (_, Left err) -> liftIO $ resp $ ErrorMessage err
    return True
  where performRefactoring cmd actualMod otherMods = do
          res <- lift $ performCommand cmd actualMod otherMods
          case res of
            Left err -> liftIO $ resp $ ErrorMessage err
            Right diff -> do changedMods <- applyChanges diff
                             liftIO $ resp $ ModulesChanged (map (either id (\(_,_,ch) -> ch)) changedMods)
                             void $ reloadChanges (map ((^. sfkModuleName) . (\(key,_,_) -> key)) (rights changedMods))
        applyChanges changes = do
          forM changes $ \case
            ModuleCreated n m otherM -> do
              mcs <- gets (^. refSessMCs)
              Just (_, otherMR) <- gets (lookupModInSCs otherM . (^. refSessMCs))

              let Just otherMS = otherMR ^? modRecMS
                  Just mc = lookupModuleColl (otherM ^. sfkModuleName) mcs
              otherSrcDir <- liftIO $ getSourceDir otherMS
              let loc = toFileName otherSrcDir n
              modify $ refSessMCs & traversal & filtered (\mc' -> (mc' ^. mcId) == (mc ^. mcId)) & mcModules
                         .- Map.insert (SourceFileKey loc n) (ModuleNotLoaded False False)
              liftIO $ withBinaryFile loc WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle (prettyPrint m)
              lift $ addTarget (Target (TargetFile loc Nothing) True Nothing)
              return $ Right (SourceFileKey loc n, loc, RemoveAdded loc)
            ContentChanged (n,m) -> do
              let newCont = prettyPrint m
                  file = n ^. sfkFileName
              origCont <- liftIO $ withBinaryFile file ReadMode $ \handle -> do
                hSetEncoding handle utf8
                StrictIO.hGetContents handle
              let undo = createUndo 0 $ getGroupedDiff origCont newCont
              origCont <- liftIO $ withBinaryFile file WriteMode $ \handle -> do
                hSetEncoding handle utf8
                hPutStr handle newCont
              return $ Right (n, file, UndoChanges file undo)
            ModuleRemoved mod -> do
              Just (sfk,_) <- gets (lookupModuleInSCs mod . (^. refSessMCs))
              let file = sfk ^. sfkFileName
              origCont <- liftIO (StrictBS.unpack <$> StrictBS.readFile file)
              lift $ removeTarget (TargetFile file Nothing)
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
      let existing = (existingMCs ^? traversal & filtered isTheAdded & mcModules & traversal & modRecMS)
          existingModNames = map ms_mod existing
      needToReload <- handleErrors $ (filter (\ms -> not $ ms_mod ms `elem` existingModNames))
                                       <$> getReachableModules (\_ -> return ()) (\ms -> ms_mod ms `elem` existingModNames)
      modify $ refSessMCs .- filter (not . isTheAdded) -- remove the added package from the database
      forM_ existing $ \ms -> removeTarget (TargetFile (getModSumOrig ms) Nothing)
      modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` existingModNames) . ms_mod) (hsc_mod_graph s) })
      -- load new modules
      pkgDBok <- initializePackageDBIfNeeded
      if pkgDBok then do
        res <- loadPackagesFrom (\ms -> resp (LoadedModules [(getModSumOrig ms, getModSumName ms)]) >> return (getModSumOrig ms))
                                (resp . LoadingModules . map getModSumOrig) (\st fp -> maybeToList <$> detectAutogen fp (st ^. packageDB)) packagePathes
        case res of
          Right modules -> do
            mapM_ (reloadModule (\_ -> return ())) (either (const []) id needToReload) -- don't report consequent reloads (not expected)
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
