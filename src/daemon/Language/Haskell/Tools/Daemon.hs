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
module Language.Haskell.Tools.Daemon where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import Data.Algorithm.Diff
import qualified Data.ByteString.Char8 as StrictBS
import Data.Either
import Data.IORef
import Data.List hiding (insert)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import Data.Version
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import System.IO.Strict as StrictIO (hGetContents)

import Bag
import DynFlags
import ErrUtils
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc, modifySession)
import HscTypes (hsc_mod_graph)
import Packages

import Language.Haskell.Tools.Daemon.Mode
import Language.Haskell.Tools.Daemon.PackageDB
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.Session
import Language.Haskell.Tools.Daemon.State
import Language.Haskell.Tools.Daemon.Utils
import Language.Haskell.Tools.Daemon.Update
import Language.Haskell.Tools.Daemon.Watch
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin
import Paths_haskell_tools_daemon

runDaemonCLI :: IO ()
runDaemonCLI = do store <- newEmptyMVar
                  getArgs >>= runDaemon builtinRefactorings socketMode store

runDaemon' :: [RefactoringChoice IdDom] -> [String] -> IO ()
runDaemon' refactorings args = do store <- newEmptyMVar
                                  runDaemon refactorings socketMode store args

runDaemon :: [RefactoringChoice IdDom] -> WorkingMode a -> MVar a -> [String] -> IO ()
runDaemon refactorings mode connStore args = withSocketsDo $
    do let finalArgs = args ++ drop (length args) defaultArgs
           isSilent = read (finalArgs !! 1)
           watchExe = if length finalArgs < 3 then Nothing
                                              else Just (finalArgs !! 2)
       hSetBuffering stdout LineBuffering
       hSetBuffering stderr LineBuffering
       when (not isSilent) $ putStrLn $ "Starting Haskell Tools daemon"
       conn <- daemonConnect mode finalArgs
       putMVar connStore conn
       when (not isSilent) $ putStrLn $ "Listening on port " ++ finalArgs !! 0
       ghcSess <- initGhcSession
       state <- newMVar initSession
       watchProcess <- case watchExe of
                         Just path -> Just <$> createWatchProcess path ghcSess state (daemonSend mode conn)
                         Nothing -> return Nothing
       modifyMVarMasked_ state ( \s -> return s { _watchProc = watchProcess })
       serverLoop refactorings mode conn isSilent ghcSess state
       case watchProcess of Just wp -> stopWatch wp
                            Nothing -> return ()
       daemonDisconnect mode conn

defaultArgs :: [String]
defaultArgs = ["4123", "True"]

serverLoop :: [RefactoringChoice IdDom] -> WorkingMode a -> a -> Bool -> Session -> MVar DaemonSessionState -> IO ()
serverLoop refactorings mode conn isSilent ghcSess state =
  ( do msgs <- daemonReceive mode conn
       continue <- forM msgs $ \case Right req -> respondTo refactorings ghcSess state (daemonSend mode conn) req
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
