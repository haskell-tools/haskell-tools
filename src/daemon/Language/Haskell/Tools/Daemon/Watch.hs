{-# LANGUAGE RecordWildCards
           , ScopedTypeVariables
           #-}
-- | Controls the file system watching in the daemon. The file system watching must run in a
-- separate process to prevent blocking because of file operations interfering with watch.
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
import System.FSWatch.Repr
import System.FSWatch.Slave

import GhcMonad (Session(..), reflectGhc)

import Language.Haskell.Tools.Daemon.Protocol (ResponseMsg, ClientMessage(..))
import Language.Haskell.Tools.Daemon.State (DaemonSessionState)
import Language.Haskell.Tools.Daemon.Update (reloadModules)

-- | Starts the watch process and a thread that receives notifications from it. The notification
-- thread will invoke updates on the daemon state to re-load files.
createWatchProcess' :: Maybe FilePath -> Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ())
                        -> IO (Maybe WatchProcess, [ThreadId])
createWatchProcess' watchExePath ghcSess daemonSess upClient = do
    exePath <- case watchExePath of Just exe -> return exe
                                    Nothing -> guessExePath
    process <- createWatchProcess exePath 100
    initProcess process
  where
    initProcess process = do
      reloaderThread <- forkIO $ forever $ void $ do
        changes <- waitNotifies process
        let changedFiles = catMaybes $ map getModifiedFile changes
            addedFiles = catMaybes $ map getAddedFile changes
            removedFiles = catMaybes $ map getRemovedFile changes
            reloadAction = reloadModules upClient addedFiles changedFiles removedFiles
        when (length changedFiles + length addedFiles + length removedFiles > 0)
          $ void $ modifyMVar daemonSess (\st -> swap <$> reflectGhc (runStateT reloadAction st) ghcSess)
      return $ (Just process, [reloaderThread])

    getModifiedFile (Mod file) = Just file
    getModifiedFile _ = Nothing

    getAddedFile (Add file) = Just file
    getAddedFile _ = Nothing

    getRemovedFile (Rem file) = Just file
    getRemovedFile _ = Nothing

    guessExePath = do exePath <- getExecutablePath
                      return $ takeDirectory exePath </> "hfswatch"

-- | Stops the watch process and all threads associated with it.
stopWatch :: WatchProcess -> [ThreadId] -> IO ()
stopWatch WatchProcess{..} threads
  = do forM threads killThread
       wShutdown
