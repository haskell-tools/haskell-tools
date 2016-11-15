{-# LANGUAGE OverloadedStrings
           , DeriveGeneric 
           , TypeApplications
           , TupleSections
           , ScopedTypeVariables
           , LambdaCase
           , TemplateHaskell
           #-}

module Language.Haskell.Tools.Demo where

import Network.WebSockets
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Control.Exception
import Data.Aeson hiding ((.=))
import Data.Map (Map, (!), member, insert)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import GHC.Generics

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import Data.IORef
import Data.List hiding (insert)
import Data.Tuple
import Data.Maybe
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
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.ASTDebug
import Language.Haskell.Tools.ASTDebug.Instances
import Language.Haskell.Tools.PrettyPrint

type ClientId = Int

data RefactorSessionState
  = RefactorSessionState { _refSessMods :: Map.Map (String, String, IsBoot) (UnnamedModule IdDom)
                         , _actualMod :: Maybe (String, String, IsBoot)
                         }

makeReferences ''RefactorSessionState

initSession :: RefactorSessionState
initSession = RefactorSessionState Map.empty Nothing

runFromCLI :: IO ()
runFromCLI = getArgs >>= runDemo

runDemo :: [String] -> IO ()
runDemo args = do
  wd <- case args of [dir] -> return dir
                     [] -> return "."
  counter <- newMVar []
  let settings = setPort 8206 $ setTimeout 20 $ defaultSettings 
  runSettings settings (app counter wd)

-- | The application that is evoked for each incoming request
app :: MVar [Int] -> FilePath -> Application
app sessions wd = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp conn = do
        conn <- acceptRequest conn
        newind <- modifyMVar sessions (\sess -> let smallest = head (filter (not . (`elem` sess)) [0..])
                                                 in return (smallest : sess, smallest))
        ghcSess <- initGhcSession (userDir wd newind)
        state <- newMVar initSession
        serverLoop newind ghcSess state conn

    serverLoop :: Int -> Session -> MVar RefactorSessionState -> Connection -> IO ()
    serverLoop sessId ghcSess state conn =
        do Text msg <- receiveDataMessage conn
           respondTo wd sessId ghcSess state (sendTextData conn) msg
           serverLoop sessId ghcSess state conn
      `catch` \(e :: ConnectionException) -> do 
                 modifyMVar_ sessions (return . delete sessId)
                 liftIO $ removeDirectoryIfPresent (userDir wd sessId)

    backupApp :: Application
    backupApp req respond = respond $ responseLBS status400 [] "Not a WebSocket request"

respondTo :: FilePath -> Int -> Session -> MVar RefactorSessionState -> (ByteString -> IO ()) -> ByteString -> IO ()
respondTo wd id ghcSess state next mess = case decode mess of
  Nothing -> next $ encode $ ErrorMessage "WRONG MESSAGE FORMAT"
  Just req -> handleErrors wd req (next . encode)
                $ do resp <- modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient (userDir wd id) req) st) ghcSess)
                     case resp of Just respMsg -> next $ encode respMsg
                                  Nothing -> return ()

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: FilePath -> ClientMessage -> StateT RefactorSessionState Ghc (Maybe ResponseMsg)
updateClient dir KeepAlive = return Nothing
updateClient dir (ModuleChanged name newContent) = do
    liftIO $ createFileForModule dir name newContent
    targets <- lift getTargets
    when (isNothing . find ((\case (TargetModule n) -> GHC.moduleNameString n == name; _ -> False) . targetId) $ targets)
      $ lift $ addTarget (Target (TargetModule (GHC.mkModuleName name)) True Nothing)
    lift $ load LoadAllTargets
    mod <- lift $ getModSummary (GHC.mkModuleName name) >>= parseTyped
    modify $ refSessMods .- Map.insert (dir, name, NormalHs) mod
    return Nothing
updateClient dir (ModuleDeleted name) = do
    lift $ removeTarget (TargetModule (GHC.mkModuleName name))
    modify $ refSessMods .- Map.delete (dir, name, NormalHs)
    return Nothing
updateClient dir (InitialProject modules) = do 
    -- clean the workspace to remove source files from earlier sessions
    liftIO $ removeDirectoryIfPresent dir
    liftIO $ createDirectoryIfMissing True dir
    liftIO $ forM modules $ \(mod, cont) -> do
      withBinaryFile (toFileName dir mod) WriteMode (`hPutStr` cont)
    lift $ setTargets (map ((\modName -> Target (TargetModule (GHC.mkModuleName modName)) True Nothing) . fst) modules)
    lift $ load LoadAllTargets
    forM (map fst modules) $ \modName -> do
      mod <- lift $ getModSummary (GHC.mkModuleName modName) >>= parseTyped
      modify $ refSessMods .- Map.insert (dir, modName, NormalHs) mod
    return Nothing
updateClient _ (PerformRefactoring "UpdateAST" modName _ _) = do
    mod <- gets (find ((modName ==) . (\(_,m,_) -> m) . fst) . Map.assocs . (^. refSessMods))
    case mod of Just (_,m) -> return $ Just $ ASTViewContent $ astDebug m
                Nothing -> return $ Just $ ErrorMessage "The module is not found"
updateClient _ (PerformRefactoring "TestErrorLogging" _ _ _) = error "This is a test"
updateClient dir (PerformRefactoring refact modName selection args) = do
    mod <- gets (find ((modName ==) . (\(_,m,_) -> m) . fst) . Map.assocs . (^. refSessMods))
    allModules <- gets (filter ((modName /=) . fst) . map moduleNameAndContent . Map.assocs . (^. refSessMods))
    let command = analyzeCommand (toFileName dir modName) refact (selection:args)
    liftIO $ putStrLn $ (toFileName dir modName)
    liftIO $ putStrLn $ maybe "" (show . getRange . snd) mod
    case mod of Just m -> do res <- lift $ performCommand command (moduleNameAndContent m) allModules 
                             case res of
                               Left err -> return $ Just $ ErrorMessage err
                               Right diff -> do applyChanges diff
                                                return $ Just $ RefactorChanges (map trfDiff diff)
                Nothing -> return $ Just $ ErrorMessage "The module is not found"
  where trfDiff (ContentChanged (name,cont)) = (name, Just (prettyPrint cont))
        trfDiff (ModuleRemoved name) = (name, Nothing)

        applyChanges 
          = mapM_ $ \case 
              ContentChanged (n,m) -> do
                liftIO $ withBinaryFile (toFileName dir n) WriteMode (`hPutStr` prettyPrint m)
                w <- gets (find ((n ==) . (\(_,m,_) -> m)) . Map.keys . (^. refSessMods))
                newm <- lift $ (parseTyped =<< loadModule dir n)
                modify $ refSessMods .- Map.insert (dir, n, NormalHs) newm
              ModuleRemoved mod -> do
                liftIO $ removeFile (toFileName dir mod)
                modify $ refSessMods .- Map.delete (dir, mod, NormalHs)

createFileForModule :: FilePath -> String -> String -> IO ()
createFileForModule dir name newContent = do
  let fname = toFileName dir name
  createDirectoryIfMissing True (takeDirectory fname)
  withBinaryFile fname WriteMode (`hPutStr` newContent) 

removeDirectoryIfPresent :: FilePath -> IO ()
removeDirectoryIfPresent dir = removeDirectoryRecursive dir `catch` \e -> if isDoesNotExistError e then return () else throwIO e

moduleNameAndContent :: ((String,String,IsBoot), mod) -> (String, mod)
moduleNameAndContent ((_,name,_), mod) = (name, mod)

dataDirs :: FilePath -> FilePath
dataDirs wd = normalise $ wd </> "demoSources"

userDir :: FilePath -> ClientId -> FilePath
userDir wd id = dataDirs wd </> show id

initGhcSession :: FilePath -> IO Session
initGhcSession workingDir = Session <$> (newIORef =<< runGhc (Just libdir) (do 
    dflags <- getSessionDynFlags
    -- don't generate any code
    setSessionDynFlags 
      $ flip gopt_set Opt_KeepRawTokenStream
      $ flip gopt_set Opt_NoHsMain
      $ dflags { importPaths = [workingDir]
               , hscTarget = HscAsm -- needed for static pointers
               , ghcLink = LinkInMemory
               , ghcMode = CompManager 
               }
    getSession))

handleErrors :: FilePath -> ClientMessage -> (ResponseMsg -> IO ()) -> IO () -> IO ()
handleErrors wd req next io = io `catch` (next <=< handleException)
  where handleException :: SomeException -> IO ResponseMsg
        handleException e 
          | Just (se :: SourceError) <- fromException e 
          = return $ CompilationProblem (concatMap (\msg -> showMsg msg ++ "\n\n") $ bagToList $ srcErrorMessages se)
          | Just (ae :: AsyncException) <- fromException e = throw ae
          | Just (ge :: GhcException) <- fromException e = return $ ErrorMessage $ show ge
          | otherwise = do logToFile wd (show e) req
                           return $ ErrorMessage (showInternalError e)
        
        showMsg msg = showSpan (errMsgSpan msg) ++ "\n" ++ show msg
        showSpan (RealSrcSpan sp) = showFileName (srcLocFile (realSrcSpanStart sp)) ++ " " ++ show (srcLocLine (realSrcSpanStart sp)) ++ ":" ++ show (srcLocCol (realSrcSpanStart sp))
        showSpan _ = ""

        showFileName = joinPath . drop 2 . splitPath . makeRelative wd . unpackFS

        showInternalError :: SomeException -> String
        showInternalError e = "An internal error happened. The report has been sent to the developers. " ++ show e

logToFile :: FilePath -> String -> ClientMessage -> IO ()
logToFile wd err input = do
  let msg = err ++ "\n with input: " ++ show input
  withFile logFile AppendMode $ \handle -> do
      size <- hFileSize handle
      when (size < logSizeLimit) $ hPutStrLn handle ("\n### " ++ msg)
    `catch` \e -> print ("The error message cannot be logged because: " 
                             ++ show (e :: IOException) ++ "\nHere is the message:\n" ++ msg) 
  where logFile = wd </> "error-log.txt"
        logSizeLimit = 100 * 1024 * 1024 -- 100 MB

data ClientMessage
  = KeepAlive
  | InitialProject { initialModules :: [(String,String)] }
  | PerformRefactoring { refactoring :: String
                       , moduleName :: String
                       , editorSelection :: String
                       , details :: [String]
                       }
  | ModuleChanged { moduleName :: String
                  , newContent :: String
                  }
  | ModuleDeleted { moduleName :: String }
  deriving (Eq, Show, Generic)

instance ToJSON ClientMessage
instance FromJSON ClientMessage 

data ResponseMsg
  = RefactorChanges { moduleChanges :: [(String, Maybe String)] }
  | ASTViewContent { astContent :: String }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseMsg
instance FromJSON ResponseMsg 
