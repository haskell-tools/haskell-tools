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
import Data.Tuple
import Data.Version
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import System.IO

import GhcMonad (Session(..), reflectGhc)

import Language.Haskell.Tools.Daemon.ErrorHandling
import Language.Haskell.Tools.Daemon.Mode
import Language.Haskell.Tools.Daemon.Options as Options
import Language.Haskell.Tools.Daemon.Protocol
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

-- | Starts the daemon process. This will not return until the daemon stops.
-- The daemon process is parameterized by the refactorings you can use in it. This entry point gives
-- back the other endpoint of the connection so it can be used to run the daemon in the same process.
runDaemon :: [RefactoringChoice IdDom] -> WorkingMode a -> MVar a -> DaemonOptions -> IO ()
runDaemon _ _ _ DaemonOptions{..} | daemonVersion
  = putStrLn $ showVersion version
runDaemon refactorings mode connStore config@DaemonOptions{..} = withSocketsDo $
    do when (not silentMode) $ putStrLn $ "Starting Haskell Tools daemon"
       hSetBuffering stdout LineBuffering
       hSetBuffering stderr LineBuffering
       conn <- daemonConnect mode portNumber
       putMVar connStore conn
       when (not silentMode) $ putStrLn $ "Connection established"
       ghcSess <- initGhcSession (generateCode sharedOptions)
       state <- newMVar initSession
       -- set the ghc flags given by command line
       case Options.ghcFlags sharedOptions of
         Just flags -> void $ respondTo config refactorings ghcSess state (daemonSend mode conn) (SetGHCFlags flags)
         Nothing -> return ()
       case projectType sharedOptions of
         Just t -> void $ respondTo config refactorings ghcSess state (daemonSend mode conn) (SetPackageDB t)
         Nothing -> return ()
       -- set up the file watch
       (wp,th) <- if noWatch sharedOptions
                    then return (Nothing, [])
                    else createWatchProcess'
                           (watchExe sharedOptions) ghcSess state (daemonSend mode conn)
       modifyMVarMasked_ state ( \s -> return s { _watchProc = wp, _watchThreads = th })
       -- start the server loop
       serverLoop refactorings mode conn config ghcSess state
       -- free allocated resources
       case wp of Just watchProcess -> stopWatch watchProcess th
                  Nothing -> return ()
       daemonDisconnect mode conn

-- | Starts the server loop, receiving requests from the client and updated the server state
-- according to these.
serverLoop :: [RefactoringChoice IdDom] -> WorkingMode a -> a -> DaemonOptions -> Session
                -> MVar DaemonSessionState -> IO ()
serverLoop refactorings mode conn options ghcSess state =
  do msgs <- daemonReceive mode conn
     continue <- mapM respondToMsg msgs
     sessionData <- readMVar state
     when (not (sessionData ^. exiting) && all (== True) continue)
       $ serverLoop refactorings mode conn options ghcSess state
   `catches` exceptionHandlers (serverLoop refactorings mode conn options ghcSess state)
                               (daemonSend mode conn . ErrorMessage)
  where respondToMsg (Right req)
          = do when (not (silentMode options)) $ putStrLn $ "Message received: " ++ show req
               respondTo options refactorings ghcSess state (daemonSend mode conn) req
           `catches` userExceptionHandlers
                        (\s -> daemonSend mode conn (ErrorMessage s) >> return True)
                        (\err hint -> daemonSend mode conn (CompilationProblem err hint) >> return True)
        respondToMsg (Left msg) = do daemonSend mode conn $ ErrorMessage $ "MALFORMED MESSAGE: " ++ msg
                                     return True

-- | Responds to a client request by modifying the daemon and GHC state accordingly.
respondTo :: DaemonOptions -> [RefactoringChoice IdDom] -> Session -> MVar DaemonSessionState
               -> (ResponseMsg -> IO ()) -> ClientMessage -> IO Bool
respondTo options refactorings ghcSess state next req
  = modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient options refactorings next req) st) ghcSess)
