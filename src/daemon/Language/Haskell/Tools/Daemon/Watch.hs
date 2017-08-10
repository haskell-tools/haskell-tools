{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Tools.Daemon.Watch where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import qualified Data.Aeson as A ()
import Data.List hiding (insert)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Tuple (swap)
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import System.FilePath (FilePath)
import System.IO
import System.Process

import GHC hiding (loadModule)
import GhcMonad (GhcMonad(..), Session(..), reflectGhc)
import Language.Haskell.Tools.Daemon.Protocol (ResponseMsg, ClientMessage(..))
import Language.Haskell.Tools.Daemon.State (WatchProcess(..), DaemonSessionState)
import Language.Haskell.Tools.Daemon.Update (updateClient)

createWatchProcess :: FilePath -> Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ()) -> IO WatchProcess
createWatchProcess watchExePath ghcSess daemonSess upClient = do
    (Just _watchStdIn, Just _watchStdOut, _, _watchPHandle)
      <- createProcess (proc watchExePath ["slave"]) { std_in = CreatePipe, std_out = CreatePipe }
    hSetBuffering _watchStdIn NoBuffering
    hSetBuffering _watchStdOut NoBuffering
    hSetNewlineMode _watchStdIn (NewlineMode LF LF)
    hSetNewlineMode _watchStdOut (NewlineMode LF LF)
    -- collects changes that appear in a given timeframe
    store <- newEmptyMVar
    collectorThread <- forkIO $ forever $ void $ do
        str <- hGetLine _watchStdOut
        put <- tryPutMVar store [str] -- when the mvar is empty (this is the first change since last reload)
        when (not put) $ modifyMVar_ store (return . (++ [str])) -- otherwise append
    reloaderThread <- forkIO $ forever $ void $ do
        firstChanges <- readMVar store
        allChanges <- accumulateChanges store firstChanges
        changedFiles <- catMaybes <$> mapM (getFiles "Mod") allChanges
        addedFiles <- catMaybes <$> mapM (getFiles "Add") allChanges
        removedFiles <- catMaybes <$> mapM (getFiles "Rem") allChanges
        let rel = ReLoad addedFiles changedFiles removedFiles
        when (length changedFiles + length addedFiles + length removedFiles > 0)
          $ void $ modifyMVar daemonSess (\st -> swap <$> reflectGhc (runStateT (updateClient [] upClient rel) st) ghcSess)
    let _watchThreads = [collectorThread, reloaderThread]
    return WatchProcess { .. }
  where accumulateChanges store previous = do
          -- TODO: make this a parameter
          threadDelay 100000 -- wait for 0.1 seconds
          changes <- readMVar store
          if changes == previous then takeMVar store
                                 else accumulateChanges store changes
        getFiles kw str =
          case words str of
            ([action, fn]) | action == kw
               -> return (Just ({- get rid of escapes and quotes -} read fn))
            _  -> return Nothing

stopWatch :: WatchProcess -> IO ()
stopWatch WatchProcess{..}
  = do hPutStrLn _watchStdIn $ "exit"
       forM _watchThreads killThread
       void $ waitForProcess _watchPHandle
