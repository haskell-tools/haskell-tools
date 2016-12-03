{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings 
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Refactor.Daemon where

import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy
import Control.Exception
import Data.Aeson hiding ((.=))
import Data.Map (Map, (!), member, insert)
import qualified Data.Map as Map
import GHC.Generics

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import Data.IORef
import Data.List hiding (insert)
import Data.Tuple
import Data.Maybe
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import System.Environment
import Debug.Trace

import GHC hiding (loadModule)
import Bag (bagToList)
import SrcLoc (realSrcSpanStart)
import ErrUtils (errMsgSpan)
import DynFlags (gopt_set)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc, modifySession)
import HscTypes (SourceError, srcErrorMessages, hsc_mod_graph)
import FastString (unpackFS)

import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.Daemon.State

-- TODO: handle boot files

runDaemonCLI :: IO ()
runDaemonCLI = getArgs >>= runDaemon

runDaemon :: [String] -> IO ()
runDaemon args = withSocketsDo $
    do let finalArgs = args ++ drop (length args) defaultArgs
           isSilent = read (finalArgs !! 1)
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just (finalArgs !! 0))
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       setSocketOption sock ReuseAddr 1
       bind sock (addrAddress serveraddr)
       listen sock 1
       clientLoop isSilent sock

defaultArgs = ["4123", "True"]

clientLoop :: Bool -> Socket -> IO ()
clientLoop isSilent sock
  = do (conn,_) <- accept sock
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
  `catch` interrupted sock
  where interrupted = \s ex -> do
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
updateClient resp KeepAlive = liftIO (resp KeepAliveResponse) >> return True
updateClient resp Disconnect = liftIO (resp Disconnected) >> return False
updateClient resp (AddPackages packagePathes) = do
    existing <- gets (map ms_mod . (^? refSessMCs & traversal & filtered isTheAdded & mcModules & traversal & modRecMS))
    needToReload <- (filter (\ms -> not $ ms_mod ms `elem` existing)) 
                      <$> getReachableModules (\ms -> ms_mod ms `elem` existing)
    modify $ refSessMCs .- filter (not . isTheAdded) -- remove the added package from the database
    forM_ existing $ \mn -> removeTarget (TargetModule (GHC.moduleName mn))
    modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` existing) . ms_mod) (hsc_mod_graph s) })
    (modules, ignoredMods) <- loadPackagesFrom (return . getModSumOrig) packagePathes
    mapM_ (reloadModule (\_ -> return ())) needToReload -- don't report consequent reloads (not expected)
    liftIO $ resp 
      $ if not (null ignoredMods) 
          then ErrorMessage 
                 $ "The following modules are ignored: " 
                     ++ concat (intersperse ", " ignoredMods)
                     ++ ". Multiple modules with the same qualified name are not supported."
          else LoadedModules modules
    return True
  where isTheAdded mc = (mc ^. mcRoot) `elem` packagePathes

updateClient resp (RemovePackages packagePathes) = do
    mcs <- gets (^. refSessMCs)
    let existing = map ms_mod (mcs ^? traversal & filtered isRemoved & mcModules & traversal & modRecMS)
    lift $ forM_ existing (\modName -> removeTarget (TargetModule (GHC.moduleName modName)))
    lift $ deregisterDirs (mcs ^? traversal & filtered isRemoved & mcSourceDirs & traversal)
    modify $ refSessMCs .- filter (not . isRemoved)
    modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` existing) . ms_mod) (hsc_mod_graph s) })
    mods <- lift getModuleGraph
    return True
  where isRemoved mc = (mc ^. mcRoot) `elem` packagePathes

updateClient resp (ReLoad changed removed) =
  do removedMods <- gets (map ms_mod . filter ((`elem` removed) . getModSumOrig) . (^? refSessMCs & traversal & mcModules & traversal & modRecMS))
     lift $ forM_ removedMods (\modName -> removeTarget (TargetModule (GHC.moduleName modName)))
     modify $ refSessMCs & traversal & mcModules 
                .- Map.filter (\m -> maybe True (not . (`elem` removed) . getModSumOrig) (m ^? modRecMS))
     modifySession (\s -> s { hsc_mod_graph = filter (not . (`elem` removedMods) . ms_mod) (hsc_mod_graph s) })
     reloadChangedModules (\ms -> resp (LoadedModules [getModSumOrig ms]))
                          (\ms -> getModSumOrig ms `elem` changed)
     return True

updateClient _ Stop = modify (exiting .= True) >> return False

updateClient resp (PerformRefactoring refact modPath selection args) = do
    (Just actualMod, otherMods) <- getFileMods modPath
    let cmd = analyzeCommand refact (selection:args)
    res <- lift $ performCommand cmd actualMod otherMods
    case res of
      Left err -> liftIO $ resp $ ErrorMessage err
      Right diff -> do changedMods <- catMaybes <$> applyChanges diff
                       liftIO $ resp $ ModulesChanged (map snd changedMods)
                       void $ reloadChanges (map ((^. sfkModuleName) . fst) changedMods)
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
              liftIO $ withBinaryFile loc WriteMode (`hPutStr` prettyPrint m)
              lift $ addTarget (Target (TargetModule (GHC.mkModuleName n)) True Nothing)
              return $ Just (SourceFileKey NormalHs n, loc)
            ContentChanged (n,m) -> do
              Just (_, mr) <- gets (lookupModInSCs n . (^. refSessMCs))
              let Just ms = mr ^? modRecMS
              liftIO $ withBinaryFile (getModSumOrig ms) WriteMode (`hPutStr` prettyPrint m)
              return $ Just (n, getModSumOrig ms)
            ModuleRemoved mod -> do
              Just (_,m) <- gets (lookupModInSCs (SourceFileKey NormalHs mod) . (^. refSessMCs))
              let modName = GHC.moduleName $ fromJust $ fmap semanticsModule (m ^? typedRecModule) <|> fmap semanticsModule (m ^? renamedRecModule)
              ms <- getModSummary modName
              lift $ removeTarget (TargetModule modName)
              modify $ (refSessMCs .- removeModule mod)
              liftIO $ removeFile (getModSumOrig ms)
              return Nothing
          
        reloadChanges changedMods 
          = reloadChangedModules (\ms -> resp $ LoadedModules [getModSumOrig ms]) (\ms -> modSumName ms `elem` changedMods)

initGhcSession :: IO Session
initGhcSession = Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags >> getSession))

data ClientMessage
  = KeepAlive
  | AddPackages { addedPathes :: [FilePath] }
  | RemovePackages { removedPathes :: [FilePath] }
  | PerformRefactoring { refactoring :: String
                       , modulePath :: FilePath
                       , editorSelection :: String
                       , details :: [String]
                       }
  | Stop
  | Disconnect
  | ReLoad { changedModules :: [FilePath]
           , removedModules :: [FilePath]
           }
  -- ReLoadAll -- re-load all modules
  -- Reset -- completely re-initialize the refactor sesson
  deriving (Show, Generic)


instance FromJSON ClientMessage 

data ResponseMsg
  = KeepAliveResponse
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  | ModulesChanged { moduleChanges :: [FilePath] }
  | LoadedModules { loadedModules :: [FilePath] }
  | Disconnected
  deriving (Show, Generic)

instance ToJSON ResponseMsg
