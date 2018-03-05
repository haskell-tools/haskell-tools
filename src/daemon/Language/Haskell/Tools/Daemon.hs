{-# LANGUAGE FlexibleContexts, MonoLocalBinds, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications #-}

-- | The central module for the background process of Haskell-tools. Starts the daemon process and
-- updates it for each client request in a loop. After this releases the resources and terminates.
module Language.Haskell.Tools.Daemon where

import Control.Concurrent.MVar
import Control.Exception (catches)
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import Data.Tuple (swap)
import Data.Version (showVersion)
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import System.IO

import GhcMonad (Session(..), reflectGhc)

import Language.Haskell.Tools.Daemon.ErrorHandling (userExceptionHandlers, exceptionHandlers)
import Language.Haskell.Tools.Daemon.Mode (WorkingMode(..), socketMode)
import Language.Haskell.Tools.Daemon.Options as Options (SharedDaemonOptions(..), DaemonOptions(..))
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Daemon.State (DaemonSessionState(..), initSession, exiting)
import Language.Haskell.Tools.Daemon.Update (updateClient, initGhcSession)
import Language.Haskell.Tools.Daemon.Watch (createWatchProcess', stopWatch)
import Language.Haskell.Tools.Refactor (RefactoringChoice(..), QueryChoice(..))
import Paths_haskell_tools_daemon (version)

-- | Starts the daemon process. This will not return until the daemon stops. You can use this entry
-- point when the other endpoint of the client connection is not needed, for example, when you use
-- socket connection to connect to the daemon process.
runDaemon' :: [RefactoringChoice] -> [QueryChoice] -> DaemonOptions -> IO ()
runDaemon' refactorings queries args
  = do store <- newEmptyMVar
       runDaemon refactorings queries socketMode store args

-- | Starts the daemon process. This will not return until the daemon stops.
-- The daemon process is parameterized by the refactorings you can use in it. This entry point gives
-- back the other endpoint of the connection so it can be used to run the daemon in the same process.
runDaemon :: [RefactoringChoice] -> [QueryChoice] -> WorkingMode a -> MVar a -> DaemonOptions -> IO ()
runDaemon _ _ _ _ DaemonOptions{..} | daemonVersion
  = putStrLn $ showVersion version
runDaemon refactorings queries mode connStore config@DaemonOptions{..} = withSocketsDo $
    do when (not silentMode) $ putStrLn $ "Starting Haskell Tools daemon"
       hSetBuffering stdout LineBuffering
       hSetBuffering stderr LineBuffering
       conn <- daemonConnect mode portNumber
       putMVar connStore conn
       when (not silentMode) $ putStrLn $ "Connection established"
       (ghcSess, warnMVar) <- initGhcSession (generateCode sharedOptions)
       state <- newMVar initSession
       -- set the ghc flags given by command line
       case Options.ghcFlags sharedOptions of
         Just flags -> void $ respondTo config refactorings queries ghcSess state (daemonSend mode conn) warnMVar (SetGHCFlags flags)
         Nothing -> return ()
       case projectType sharedOptions of
         Just t -> void $ respondTo config refactorings queries ghcSess state (daemonSend mode conn) warnMVar (SetPackageDB t)
         Nothing -> return ()
       -- set up the file watch
       (wp,th) <- if noWatch sharedOptions
                    then return (Nothing, [])
                    else createWatchProcess'
                           (watchExe sharedOptions) ghcSess state warnMVar (daemonSend mode conn)
       modifyMVarMasked_ state ( \s -> return s { _watchProc = wp, _watchThreads = th })
       -- start the server loop
       serverLoop refactorings queries mode conn config ghcSess state warnMVar
       -- free allocated resources
       case wp of Just watchProcess -> stopWatch watchProcess th
                  Nothing -> return ()
       daemonDisconnect mode conn

-- | Starts the server loop, receiving requests from the client and updated the server state
-- according to these.
serverLoop :: [RefactoringChoice] -> [QueryChoice] -> WorkingMode a -> a -> DaemonOptions -> Session
                -> MVar DaemonSessionState -> MVar [Marker] -> IO ()
serverLoop refactorings queries mode conn options ghcSess state warnMVar =
  do msgs <- daemonReceive mode conn
     continue <- mapM respondToMsg msgs
     sessionData <- readMVar state
     when (not (sessionData ^. exiting) && all (== True) continue)
       $ serverLoop refactorings queries mode conn options ghcSess state warnMVar
   `catches` exceptionHandlers (serverLoop refactorings queries mode conn options ghcSess state warnMVar)
                               (daemonSend mode conn . ErrorMessage)
  where respondToMsg (Right req)
          = do when (not (silentMode options)) $ putStrLn $ "Message received: " ++ show req
               respondTo options refactorings queries ghcSess state (daemonSend mode conn) warnMVar req
           `catches` userExceptionHandlers
                        (\s -> daemonSend mode conn (ErrorMessage s) >> return True)
                        (\err hint -> daemonSend mode conn (CompilationProblem err hint) >> return True)
        respondToMsg (Left msg) = do daemonSend mode conn $ ErrorMessage $ "MALFORMED MESSAGE: " ++ msg
                                     return True

-- | Responds to a client request by modifying the daemon and GHC state accordingly.
respondTo :: DaemonOptions -> [RefactoringChoice] -> [QueryChoice] -> Session
               -> MVar DaemonSessionState -> (ResponseMsg -> IO ()) -> MVar [Marker]
               -> ClientMessage -> IO Bool
respondTo options refactorings queries ghcSess state next warnMVar req
  = modifyMVar state (\st -> swap <$> reflectGhc (runStateT upClient st) ghcSess)
  where upClient = updateClient options warnMVar refactorings queries next req

