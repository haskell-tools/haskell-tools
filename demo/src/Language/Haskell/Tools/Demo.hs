{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , TypeApplications
           , TupleSections
           , ScopedTypeVariables
           , LambdaCase
           , TemplateHaskell
           , PatternGuards
           , FlexibleContexts
           #-}

module Language.Haskell.Tools.Demo where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.IORef
import Data.List hiding (insert)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error

import Bag (bagToList)
import ErrUtils (errMsgSpan)
import FastString (unpackFS)
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc)
import HscTypes (SourceError, srcErrorMessages)
import SrcLoc (realSrcSpanStart)

import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.ASTDebug
import Language.Haskell.Tools.ASTDebug.Instances ()
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor hiding (initSession)
import Language.Haskell.Tools.Refactor.Builtin

type ClientId = Int

data RefactorSessionState
  = RefactorSessionState { _refSessMods :: Map.Map (String, String, FilePath) (UnnamedModule IdDom)
                         , _actualMod :: Maybe (String, String, FilePath)
                         , _isDisconnecting :: Bool
                         }

makeReferences ''RefactorSessionState

initSession :: RefactorSessionState
initSession = RefactorSessionState Map.empty Nothing False

runFromCLI :: IO ()
runFromCLI = getArgs >>= runDemo

runDemo :: [String] -> IO ()
runDemo args = do
  wd <- case args of dir:_ -> return dir
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
        do Text msg _ <- receiveDataMessage conn
           respondTo wd sessId ghcSess state (sendTextData conn) msg
           currState <- readMVar state
           if currState ^. isDisconnecting
             then sendClose conn ("" :: ByteString)
             else serverLoop sessId ghcSess state conn
      `catch` \(_ :: ConnectionException) -> do
                 modifyMVar_ sessions (return . delete sessId)
                 liftIO $ removeDirectoryIfPresent (userDir wd sessId)

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

respondTo :: FilePath -> Int -> Session -> MVar RefactorSessionState -> (ByteString -> IO ()) -> ByteString -> IO ()
respondTo wd id ghcSess state next mess = case decode mess of
  Nothing -> next $ encode (ErrorMessage $ "WRONG MESSAGE FORMAT: " ++ show (BS.unpack mess))
  Just req -> handleErrors wd req (next . encode)
                $ do resp <- modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient (userDir wd id) req) st) ghcSess)
                     case resp of Just respMsg -> next $ encode respMsg
                                  Nothing -> return ()

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: FilePath -> ClientMessage -> StateT RefactorSessionState Ghc (Maybe ResponseMsg)
updateClient _ KeepAlive = return Nothing
updateClient _ Disconnect = do modify $ isDisconnecting .= True
                               return $ Just Disconnected
updateClient dir (ModuleChanged name newContent) = do
    liftIO $ createFileForModule dir name newContent
    targets <- lift getTargets
    when (isNothing . find ((\case (TargetModule n) -> GHC.moduleNameString n == name; _ -> False) . targetId) $ targets)
      $ lift $ addTarget (Target (TargetModule (GHC.mkModuleName name)) True Nothing)
    void $ lift $ load LoadAllTargets
    reloadAllMods dir
    return Nothing
updateClient dir (ModuleDeleted name) = do
    lift $ removeTarget (TargetModule (GHC.mkModuleName name))
    modify $ refSessMods .- Map.delete (dir, name, dir </> moduleSourceFile name)
    return Nothing
updateClient dir (InitialProject modules) = do
    -- clean the workspace to remove source files from earlier sessions
    liftIO $ removeDirectoryIfPresent dir
    liftIO $ createDirectoryIfMissing True dir
    liftIO $ forM_ modules $ \(mod, cont) -> do
      withBinaryFile (toFileName dir mod) WriteMode (`hPutStr` cont)
    lift $ setTargets (map ((\modName -> Target (TargetModule (GHC.mkModuleName modName)) True Nothing) . fst) modules)
    reloadAllMods dir
    return Nothing
updateClient _ (PerformRefactoring "UpdateAST" modName _ _) = do
    mod <- gets (find ((modName ==) . (\(_,m,_) -> m) . fst) . Map.assocs . (^. refSessMods))
    case mod of Just (_,m) -> return $ Just $ ASTViewContent $ astDebug m
                Nothing -> return $ Just $ ErrorMessage "The module is not found"
updateClient _ (PerformRefactoring "TestErrorLogging" _ _ _) = error "This is a test"
updateClient dir (PerformRefactoring refact modName selection args) = do
    mod <- gets (find ((modName ==) . (\(_,m,_) -> m) . fst) . Map.assocs . (^. refSessMods))
    otherModules <- gets (filter ((modName /=) . (^. sfkModuleName) . fst) . map moduleNameAndContent . Map.assocs . (^. refSessMods))

    case mod of
      Just m ->
        do res <- lift $ performCommand builtinRefactorings
                                        ([refact,selection] ++ args)
                                        (Right $ moduleNameAndContent m) otherModules
           case res of
             Right diff -> do applyChanges diff
                              return $ Just $ RefactorChanges (map trfDiff diff)
             Left err -> return $ Just $ ErrorMessage err
      Nothing -> return $ Just $ ErrorMessage "The module is not found"
  where trfDiff (ContentChanged (key,cont)) = (key ^. sfkModuleName, Just (prettyPrint cont))
        trfDiff (ModuleCreated name mod _) = (name, Just (prettyPrint mod))
        trfDiff (ModuleRemoved name) = (name, Nothing)

        applyChanges diff
          = do forM_ diff $ \case
                 ModuleCreated n m _ -> do
                   writeModule n m
                   lift $ addTarget (Target (TargetModule (GHC.mkModuleName n)) True Nothing)
                 ContentChanged (n,m) ->
                   writeModule (n ^. sfkModuleName) m
                 ModuleRemoved mod -> do
                   liftIO $ removeFile (toFileName dir mod)
                   modify $ refSessMods .- Map.delete (dir, mod, dir </> moduleSourceFile mod)
                   lift $ removeTarget (TargetModule (GHC.mkModuleName mod))
               reloadAllMods dir

        writeModule n m = liftIO $ withBinaryFile (toFileName dir n) WriteMode (`hPutStr` prettyPrint m)

reloadAllMods :: FilePath -> StateT RefactorSessionState Ghc ()
reloadAllMods dir = do
  void $ lift $ load LoadAllTargets
  targets <- lift getTargets
  forM_ (map ((\case (TargetModule n) -> n) . targetId) targets) $ \modName -> do
      mod <- lift $ getModSummary modName >>= parseTyped
      modify $ refSessMods .- Map.insert (dir, GHC.moduleNameString modName, dir </> moduleSourceFile (GHC.moduleNameString modName)) mod

createFileForModule :: FilePath -> String -> String -> IO ()
createFileForModule dir name newContent = do
  let fname = toFileName dir name
  createDirectoryIfMissing True (takeDirectory fname)
  withBinaryFile fname WriteMode (`hPutStr` newContent)

removeDirectoryIfPresent :: FilePath -> IO ()
removeDirectoryIfPresent dir = removeDirectoryRecursive dir `catch` \e -> if isDoesNotExistError e then return () else throwIO e

moduleNameAndContent :: ((String,String,FilePath), mod) -> (SourceFileKey, mod)
moduleNameAndContent ((_,name,isBoot), mod) = (SourceFileKey isBoot name, mod)

dataDirs :: FilePath -> FilePath
dataDirs wd = normalise $ wd </> "demoSources"

userDir :: FilePath -> ClientId -> FilePath
userDir wd id = dataDirs wd </> show id

initGhcSession :: FilePath -> IO Session
initGhcSession workingDir
  = Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlagsForTest >> useDirs [workingDir] >> getSession))

handleErrors :: FilePath -> ClientMessage -> (ResponseMsg -> IO ()) -> IO () -> IO ()
handleErrors wd req next io = io `catch` (next <=< handleException)
  where handleException :: SomeException -> IO ResponseMsg
        handleException e
          | Just (se :: SourceError) <- fromException e
          = if isReloading
              then do logToFile wd (show e) req
                      return $ ErrorMessage ("The generated code cannot be compiled. The problem had been reported. Please restart the demo or correct the results manually.")
              else return $ CompilationProblem (concatMap (\msg -> showMsg msg ++ "\n\n") $ bagToList $ srcErrorMessages se)
          | Just (ae :: AsyncException) <- fromException e = throw ae
          | Just (ge :: GhcException) <- fromException e = return $ ErrorMessage $ show ge
          | Just (re :: RefactorException) <- fromException e = return $ ErrorMessage $ displayException re
          | otherwise = do logToFile wd (show e) req
                           return $ ErrorMessage (showInternalError e)

        showMsg msg = showSpan (errMsgSpan msg) ++ "\n" ++ show msg
        showSpan (RealSrcSpan sp) = showFileName (srcLocFile (realSrcSpanStart sp)) ++ " " ++ show (srcLocLine (realSrcSpanStart sp)) ++ ":" ++ show (srcLocCol (realSrcSpanStart sp))
        showSpan _ = ""

        isReloading = case req of PerformRefactoring {} -> True; _ -> False

        showFileName = joinPath . drop 2 . splitPath . makeRelative wd . unpackFS

        showInternalError :: SomeException -> String
        showInternalError e = "An internal error happened. The report has been sent to the developers. " ++ displayException e

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
  | Disconnect
  deriving (Show, Generic)

instance FromJSON ClientMessage

data ResponseMsg
  = RefactorChanges { moduleChanges :: [(String, Maybe String)] }
  | ASTViewContent { astContent :: String }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  | Disconnected
  deriving (Show, Generic)

instance ToJSON ResponseMsg
