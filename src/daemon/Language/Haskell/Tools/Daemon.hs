{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           , MultiWayIf
           , TypeApplications
           , TypeFamilies
           , RecordWildCards
           #-}
-- | The central module for the background process of Haskell-tools. Starts the daemon process and
-- updates it for each client request in a loop. After this releases the resources and terminates.
module Language.Haskell.Tools.Daemon where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Version
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import System.IO
import System.IO.Error

import GhcMonad (Session(..), reflectGhc)
import SrcLoc
import Bag
import ErrUtils (ErrMsg(..))
import HscTypes

import Language.Haskell.Tools.Daemon.Mode
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.GetModules
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Update
import Language.Haskell.Tools.Daemon.Watch
import Language.Haskell.Tools.Refactor
import Paths_haskell_tools_daemon

-- | Starts the daemon process. This will not return until the daemon stops. You can use this entry
-- point when the other endpoint of the client connection is not needed, for example, when you use
-- socket connection to connect to the daemon process.
runDaemon' :: [RefactoringChoice IdDom] -> DaemonOptions -> IO ()
runDaemon' refactorings args = do store <- newEmptyMVar
                                  runDaemon refactorings socketMode store args

-- | Command line options for the daemon process.
data DaemonOptions = DaemonOptions { daemonVersion :: Bool
                                   , portNumber :: Int
                                   , silentMode :: Bool
                                   , noWatch :: Bool
                                   , watchExe :: Maybe FilePath
                                   }

-- | Starts the daemon process. This will not return until the daemon stops.
-- The daemon process is parameterized by the refactorings you can use in it. This entry point gives
-- back the other endpoint of the connection so it can be used to run the daemon in the same process.
runDaemon :: [RefactoringChoice IdDom] -> WorkingMode a -> MVar a -> DaemonOptions -> IO ()
runDaemon _ _ _ DaemonOptions{..} | daemonVersion
  = putStrLn $ showVersion version
runDaemon refactorings mode connStore DaemonOptions{..} = withSocketsDo $
    do when (not silentMode) $ putStrLn $ "Starting Haskell Tools daemon"
       conn <- daemonConnect mode portNumber
       putMVar connStore conn
       when (not silentMode) $ putStrLn $ "Connection established"
       ghcSess <- initGhcSession
       state <- newMVar initSession
       (wp,th) <- if noWatch then return (Nothing, [])
                             else createWatchProcess' watchExe ghcSess state (daemonSend mode conn)
       modifyMVarMasked_ state ( \s -> return s { _watchProc = wp, _watchThreads = th })
       serverLoop refactorings mode conn silentMode ghcSess state
       case wp of Just watchProcess -> stopWatch watchProcess th
                  Nothing -> return ()
       daemonDisconnect mode conn

-- | Starts the server loop, receiving requests from the client and updated the server state
-- according to these.
serverLoop :: [RefactoringChoice IdDom] -> WorkingMode a -> a -> Bool -> Session
                -> MVar DaemonSessionState -> IO ()
serverLoop refactorings mode conn isSilent ghcSess state =
  do msgs <- daemonReceive mode conn
     continue <- mapM respondToMsg msgs
     sessionData <- readMVar state
     when (not (sessionData ^. exiting) && all (== True) continue)
       $ serverLoop refactorings mode conn isSilent ghcSess state
   `catches` exceptionHandlers (serverLoop refactorings mode conn isSilent ghcSess state)
                               (daemonSend mode conn . ErrorMessage)
  where respondToMsg (Right req)
          = do when (not isSilent) $ putStrLn $ "Message received: " ++ show req
               respondTo refactorings ghcSess state (daemonSend mode conn) req
           `catches` userExceptionHandlers
                        (\s -> daemonSend mode conn (ErrorMessage s) >> return True)
                        (\err hint -> daemonSend mode conn (CompilationProblem err hint) >> return True)
        respondToMsg (Left msg) = do daemonSend mode conn $ ErrorMessage $ "MALFORMED MESSAGE: " ++ msg
                                     return True

-- | Responds to a client request by modifying the daemon and GHC state accordingly.
respondTo :: [RefactoringChoice IdDom] ->  Session -> MVar DaemonSessionState
               -> (ResponseMsg -> IO ()) -> ClientMessage -> IO Bool
respondTo refactorings ghcSess state next req
  = modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient refactorings next req) st) ghcSess)

userExceptionHandlers :: (String -> IO Bool) -> ([(SrcSpan, String)] -> [String] -> IO Bool) -> [Handler Bool]
userExceptionHandlers sendError sendCompProblems =
  [ Handler (\(UnsupportedPackage e) -> sendError ("There are unsupported elements in your package: " ++ e ++ " please correct them before loading them into Haskell-tools."))
  , Handler (\(UnsupportedExtension e) -> sendError ("The extension you use is not supported: " ++ e ++ ". Please check your source and cabal files for the use of that language extension."))
  , Handler (\(SpliceInsertionProblem rng _) -> sendError ("A problem occurred while type-checking the Template Haskell splice at: " ++ shortShowSpan rng ++ ". Some complex splices cannot be type checked for reasons currently unknown. Please simplify the splice. We are working on this problem."))
  , Handler (\case (BreakUpProblem outer rng _) -> sendError ("The program element at " ++ (if isGoodSrcSpan rng then shortShowSpan rng else shortShowSpan (RealSrcSpan outer)) ++ " could not be prepared for refactoring. The most likely reason is preprocessor usage. Only conditional compilation is supported, includes and preprocessor macros are not. If there is no preprocessor usage at the given location, there might be a weirdly placed comment causing a problem."))
  , Handler (\(TransformationProblem msg) -> sendError ("A problem occurred while preparing the program for refactoring: " ++ msg))
  , Handler (\(PrettyPrintProblem msg) -> sendError ("A problem occurred while pretty printing the result of the refactoring: " ++ msg))
  , Handler (\case (ConvertionProblem rng msg) -> sendError ("An unexpected problem occurred while converting the representation of the program element at " ++ shortShowSpan rng ++ ": " ++ msg)
                   (UnrootedConvertionProblem msg) -> sendError ("An unexpected problem occurred while converting between different program representations: " ++ msg))
  , Handler (\errs -> let msgs = map (\err -> (errMsgSpan err, show err))
                                   $ bagToList $ srcErrorMessages errs
                          hints = nub $ sort $ catMaybes $ map (handleSourceProblem . snd) msgs
                       in sendCompProblems msgs hints)
  ]

exceptionHandlers :: IO () -> (String -> IO ()) -> [Handler ()]
exceptionHandlers cont sendError =
  [ Handler (\(err :: IOException) -> hPutStrLn stderr $ "IO Exception caught: " ++ show err)
  , Handler (\(e :: AsyncException) -> hPutStrLn stderr $ "Asynch exception caught: " ++ show e)
  , Handler (\(ex :: SomeException) -> handleException ex cont)
  ]
  where handleException ex cont
          = case handleGHCException (show ex) of
              Nothing -> do hPutStrLn stderr $ "Unexpected error: " ++ show (ex :: SomeException)
                            sendError $ "Internal error: " ++ show ex
              Just (msg, doContinue) -> sendError msg >> when doContinue cont

handleGHCException :: String -> Maybe (String, Bool)
handleGHCException msg | "failed" `isInfixOf` msg && "C pre-processor" `isInfixOf` msg
  = Just ("Failed to load the package. The cause of that is a failure of the pre-processor. "
             ++ " Only conditional compilation is supported, includes and preprocessor macros are not: " ++ msg, False)
handleGHCException msg | "failed" `isInfixOf` msg && "Literate pre-processor" `isInfixOf` msg
  = Just ("Haskell-tools does not handle Literate Haskell yet."
             ++ " If you get this error after refactoring, you should undo the refactoring. The error message: " ++ msg, True)
handleGHCException msg = Nothing

handleSourceProblem :: String -> Maybe String
handleSourceProblem msg | "is a package module" `isInfixOf` msg
  = Just $ "A module is not found, check that the current working directory is the root of the module hierarchy. "
             ++ " Also check that none of the modules are generated by parser generators or hsc files."
handleSourceProblem msg | "Failed to load interface" `isInfixOf` msg
  = Just $ "Some of the required (external) modules cannot be loaded, because they are not found. Make sure that the project is "
              ++ " already built, and that it is built using the same package database as it is used for refactoring."
handleSourceProblem msg | "Ambiguous interface" `isInfixOf` msg
  = Just $ "Some of the required (external) modules cannot be loaded, because they are ambiguous. "
             ++ "Since there is no separation between packages and package components, make sure that you do not depend "
             ++ "on packages that contain modules with the same qualified name."
handleSourceProblem _
  = Just $ "While loading we found a compilation error in the source code. If it compiles normally "
              ++ "using cabal or stack the problem might result from modules with same name, or "
              ++ "generated files that are not up-to-date."
