{-# LANGUAGE RecordWildCards
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.Daemon.Watch where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Aeson as A ()
import Data.Maybe (Maybe(..), catMaybes)
import Data.Tuple (swap)
import System.FilePath
import System.IO
import System.Process
import System.Environment
import System.Directory

import GhcMonad (Session(..), reflectGhc)

import Language.Haskell.Tools.Daemon.Protocol (ResponseMsg, ClientMessage(..))
import Language.Haskell.Tools.Daemon.State (WatchProcess(..), DaemonSessionState)
import Language.Haskell.Tools.Daemon.Update (updateClient)

createWatchProcess :: Maybe FilePath -> Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ()) -> IO (Maybe WatchProcess)
createWatchProcess watchExePath ghcSess daemonSess upClient = do
    exePath <- case watchExePath of Just exe -> return exe
                                    Nothing -> guessExePath
    process
      <- try $ createProcess (proc exePath ["slave"]) { std_in = CreatePipe, std_out = CreatePipe }
    case process of
      Right p -> initProcess p
      Left (e :: SomeException) ->
        do putStrLn $ "Couldn't start watch process " ++ show exePath ++ ": " ++ show e
           return Nothing
  where
    initProcess (Just _watchStdIn, Just _watchStdOut, _, _watchPHandle) = do
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
      return $ Just WatchProcess{..}
    accumulateChanges store previous = do
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
    guessExePath = do exePath <- getExecutablePath
                      return $ takeDirectory exePath </> "watch"

stopWatch :: WatchProcess -> IO ()
stopWatch WatchProcess{..}
  = do hPutStrLn _watchStdIn $ "exit"
       forM _watchThreads killThread
       void $ waitForProcess _watchPHandle
