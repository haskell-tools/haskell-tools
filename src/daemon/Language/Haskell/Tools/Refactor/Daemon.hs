{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings 
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           #-}
module Language.Haskell.Tools.Refactor.Daemon where

import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
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

import GHC hiding (loadModule)
import Bag (bagToList)
import SrcLoc (realSrcSpanStart)
import ErrUtils (errMsgSpan)
import DynFlags (gopt_set)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc)
import HscTypes (SourceError, srcErrorMessages)
import FastString (unpackFS)

import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session
import Language.Haskell.Tools.PrettyPrint

-- TODO: find out which modules have changed
-- TODO: exit
-- TODO: handle boot files
-- TODO: handle multiple modules in different packages with the same module name


runDaemonCLI :: IO ()
runDaemonCLI = getArgs >>= runDaemon

runDaemon :: [String] -> IO ()
runDaemon _ = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just "4123")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       setSocketOption sock ReuseAddr 1
       bind sock (addrAddress serveraddr)
       listen sock 1
       clientLoop sock

clientLoop :: Socket -> IO ()
clientLoop sock
  = do (conn,_) <- accept sock
       ghcSess <- initGhcSession
       state <- newMVar initSession
       serverLoop ghcSess state conn
       clientLoop sock

serverLoop :: Session -> MVar RefactorSessionState -> Socket -> IO ()
serverLoop ghcSess state sock =
    do msg <- recv sock 1024
       putStrLn $ "message received: " ++ unpack msg
       respondTo ghcSess state (sendAll sock) msg
       serverLoop ghcSess state sock
  `catch` interrupted sock
  where interrupted = \s ex -> do
                        let err = show (ex :: IOException)
                        putStrLn "Closing down socket"
                        hPutStrLn stderr $ "Some exception caught: " ++ err

respondTo :: Session -> MVar RefactorSessionState -> (ByteString -> IO ()) -> ByteString -> IO ()
respondTo ghcSess state next mess = case decode mess of
  Nothing -> next $ encode $ ErrorMessage "WRONG MESSAGE FORMAT"
  Just req -> do resp <- modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient req) st) ghcSess)
                 case resp of Just respMsg -> next $ encode respMsg
                              Nothing -> return ()

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: ClientMessage -> StateT RefactorSessionState Ghc (Maybe ResponseMsg)
updateClient KeepAlive = return $ Just KeepAliveResponse
updateClient (AddPackages packagePathes) = do 
    (modules, ignoredMods) <- loadPackagesFrom return packagePathes
    return $ Just $ if not (null ignoredMods) 
      then ErrorMessage 
             $ "The following modules are ignored: " 
                 ++ concat (intersperse ", " $ map (\(id,mod) -> mod ++ " (from " ++ moduleCollectionIdString id ++ ")") ignoredMods)
                 ++ ". Multiple modules with the same qualified name are not supported."
      else LoadedModules modules

updateClient ReLoad = do
    lift $ load LoadAllTargets
    -- mod <- lift $ getModSummary (GHC.mkModuleName name) >>= parseTyped
    -- modify $ refSessMods .- Map.insert (name, NormalHs) mod
    return Nothing
-- updateClient _ Stop = return () 

updateClient (PerformRefactoring refact modName selection args) = do
    (Just actualMod, otherMods) <- getMods (Just $ SourceFileKey NormalHs modName)
    let cmd = analyzeCommand refact (selection:args)
    res <- lift $ performCommand cmd (assocToNamedMod actualMod) (map assocToNamedMod otherMods)
    case res of
      Left err -> return $ Just $ ErrorMessage err
      Right diff -> do applyChanges diff
                       return $ Just $ RefactorChanges (map trfDiff diff)
  where trfDiff (ContentChanged (name,_)) = name
        trfDiff (ModuleRemoved name) = name

        applyChanges changes = do 
          changedMods <- forM changes $ \case 
            ContentChanged (n,m) -> do
              ms <- lift $ getModSummary (mkModuleName n)
              let file = fromJust $ ml_hs_file $ ms_location ms
              liftIO $ withBinaryFile file WriteMode (`hPutStr` prettyPrint m)
              return n
            ModuleRemoved mod -> do
              Just (_,m) <- gets (lookupModInSCs (SourceFileKey NormalHs mod) . (^. refSessMCs))
              let modName = fmap GHC.moduleName (fmap semanticsModule (m ^? typedRecModule) <|> fmap semanticsModule (m ^? renamedRecModule))
              case modName of 
                Just mn -> do
                  ms <- getModSummary mn
                  let file = fromJust $ ml_hs_file $ ms_location ms
                  modify $ (refSessMCs .- removeModule mod)
                  liftIO $ removeFile file
                Nothing -> return ()
              return mod
          reloadChangedModules (\_ -> return ()) changedMods

getModuleFilePath :: UnnamedModule IdDom -> Ghc FilePath
getModuleFilePath mod = do
  let modName = GHC.moduleName $ semanticsModule mod 
  ms <- getModSummary modName
  return $ fromMaybe (error "Module location not found: " ++ GHC.moduleNameString modName) $ ml_hs_file $ ms_location ms

createFileForModule :: FilePath -> String -> String -> IO ()
createFileForModule dir name newContent = do
  let fname = toFileName dir name
  createDirectoryIfMissing True (takeDirectory fname)
  withBinaryFile fname WriteMode (`hPutStr` newContent) 

removeDirectoryIfPresent :: FilePath -> IO ()
removeDirectoryIfPresent dir = removeDirectoryRecursive dir `catch` \e -> if isDoesNotExistError e then return () else throwIO e

moduleNameAndContent :: ((String,IsBoot), mod) -> (String, mod)
moduleNameAndContent ((name,_), mod) = (name, mod)

initGhcSession :: IO Session
initGhcSession = Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags >> getSession))

data ClientMessage
  = KeepAlive
  | AddPackages { addedPathes :: [FilePath] }
  -- | RemovePackage { removedPathes :: [FilePath] }
  | PerformRefactoring { refactoring :: String
                       , moduleName :: String
                       , editorSelection :: String
                       , details :: [String]
                       }
  -- | Stop
  | ReLoad
  deriving (Eq, Show, Generic)

instance ToJSON ClientMessage
instance FromJSON ClientMessage 

data ResponseMsg
  = KeepAliveResponse
  | RefactorChanges { moduleChanges :: [String] }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  | LoadedModules { loadedModules :: [String] }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseMsg
instance FromJSON ResponseMsg 
