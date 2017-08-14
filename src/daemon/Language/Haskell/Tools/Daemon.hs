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
import System.IO.Error

import GhcMonad (Session(..), reflectGhc)

import Language.Haskell.Tools.Daemon.Mode
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Update
import Language.Haskell.Tools.Daemon.Watch
import Language.Haskell.Tools.Refactor
import Paths_haskell_tools_daemon

runDaemon' :: [RefactoringChoice IdDom] -> DaemonOptions -> IO ()
runDaemon' refactorings args = do store <- newEmptyMVar
                                  runDaemon refactorings socketMode store args

data DaemonOptions = DaemonOptions { daemonVersion :: Bool
                                   , portNumber :: Int
                                   , silentMode :: Bool
                                   , noWatch :: Bool
                                   , watchExe :: Maybe FilePath
                                   }

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
       wp <- if noWatch then return Nothing
                        else createWatchProcess watchExe ghcSess state (daemonSend mode conn)
       modifyMVarMasked_ state ( \s -> return s { _watchProc = wp })
       serverLoop refactorings mode conn silentMode ghcSess state
       case wp of Just watchProcess -> stopWatch watchProcess
                  Nothing -> return ()
       daemonDisconnect mode conn

serverLoop :: [RefactoringChoice IdDom] -> WorkingMode a -> a -> Bool -> Session -> MVar DaemonSessionState -> IO ()
serverLoop refactorings mode conn isSilent ghcSess state =
  ( do msgs <- daemonReceive mode conn
       continue <- forM msgs $ \case Right req -> do when (not isSilent) $ putStrLn $ "Message received: " ++ show req
                                                     respondTo refactorings ghcSess state (daemonSend mode conn) req
                                     Left msg -> do daemonSend mode conn $ ErrorMessage $ "MALFORMED MESSAGE: " ++ msg
                                                    return True
       sessionData <- readMVar state
       when (not (sessionData ^. exiting) && all (== True) continue)
         $ serverLoop refactorings mode conn isSilent ghcSess state
  `catchIOError` handleIOError )
  `catch` (\e -> handleException e >> serverLoop refactorings mode conn isSilent ghcSess state)
  where handleIOError err = hPutStrLn stderr $ "IO Exception caught: " ++ show err
        handleException ex = do
          let err = show (ex :: SomeException)
          hPutStrLn stderr $ "Exception caught: " ++ err
          daemonSend mode conn $ ErrorMessage $ "Internal error: " ++ err

respondTo :: [RefactoringChoice IdDom] ->  Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ()) -> ClientMessage -> IO Bool
respondTo refactorings ghcSess state next req
  = modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient refactorings next req) st) ghcSess)
