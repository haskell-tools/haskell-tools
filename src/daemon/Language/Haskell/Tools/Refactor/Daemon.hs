{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Tools.Refactor.Daemon where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
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
import Data.Version
import GHC.Generics
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Strict as StrictIO (hGetContents)
import System.Process

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
import Language.Haskell.Tools.Refactor.Daemon.Mode
import Language.Haskell.Tools.Refactor.Daemon.PackageDB
import Language.Haskell.Tools.Refactor.Daemon.Protocol
import Language.Haskell.Tools.Refactor.Daemon.State
import Language.Haskell.Tools.Refactor.Daemon.Update
import Language.Haskell.Tools.Refactor.Daemon.Watch
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
       serverLoop mode conn isSilent ghcSess state
       case watchProcess of Just wp -> stopWatch wp
                            Nothing -> return ()
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
