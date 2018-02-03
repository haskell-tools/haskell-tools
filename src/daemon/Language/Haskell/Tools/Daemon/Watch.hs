{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

-- | Controls the file system watching in the daemon. The file system watching must run in a
-- separate process to prevent blocking because of file operations interfering with watch.
module Language.Haskell.Tools.Daemon.Watch where

import Control.Concurrent
import Control.Exception (catches)
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Aeson as A ()
import Data.Maybe (Maybe(..), catMaybes, isNothing)
import Data.Tuple (swap)
import GhcMonad (Session(..), reflectGhc)
import System.Environment (getExecutablePath)
import System.FSWatch.Repr (WatchProcess(..), PE(..))
import System.FSWatch.Slave (waitNotifies, createWatchProcess)
import System.FilePath
import System.IO (IO, FilePath)

import Language.Haskell.Tools.Daemon.ErrorHandling (userExceptionHandlers, exceptionHandlers)
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Daemon.State (DaemonSessionState)
import Language.Haskell.Tools.Daemon.Update (updateForFileChanges)

-- | Starts the watch process and a thread that receives notifications from it. The notification
-- thread will invoke updates on the daemon state to re-load files.
createWatchProcess' :: Maybe FilePath -> Session -> MVar DaemonSessionState -> MVar [Marker] -> (ResponseMsg -> IO ())
                        -> IO (Maybe WatchProcess, [ThreadId])
createWatchProcess' watchExePath ghcSess daemonSess warnMVars upClient = do
    exePath <- case watchExePath of Just exe -> return exe
                                    Nothing -> guessExePath
    process <- createWatchProcess exePath 500
    initProcess process
  where
    initProcess process = do
      reloaderThread <- forkIO $ forever $ void $ do
        changes <- waitForChanges process
        putStrLn $ "changes: " ++ show changes
        let changedFiles = catMaybes $ map getModifiedFile changes
            addedFiles = catMaybes $ map getAddedFile changes
            removedFiles = catMaybes $ map getRemovedFile changes
            reloadAction = updateForFileChanges upClient warnMVars addedFiles changedFiles removedFiles
            handlers = userExceptionHandlers
                           (upClient . ErrorMessage)
                           (\err hint -> upClient (CompilationProblem err hint))
                         ++ exceptionHandlers (return ()) (upClient . ErrorMessage)
        when (length changedFiles + length addedFiles + length removedFiles > 0)
          (void (modifyMVar daemonSess (\st -> swap <$> reflectGhc (runStateT reloadAction st) ghcSess))
             `catches` handlers)
      return $ (Just process, [reloaderThread])

    waitForChanges process = do
      changes <- waitNotifies process
      refactoring <- isNothing <$> tryReadMVar daemonSess
      -- if a refactoring is in progress, we should wait for all the changes to appear
      if refactoring then (changes ++) <$> waitForChanges process
                     else return changes

    getModifiedFile (Mod file) | takeExtension file `elem` sourceExtensions = Just file
    getModifiedFile _ = Nothing

    getAddedFile (Add file) | takeExtension file `elem` sourceExtensions = Just file
    getAddedFile _ = Nothing

    getRemovedFile (Rem file) | takeExtension file `elem` sourceExtensions = Just file
    getRemovedFile _ = Nothing

    sourceExtensions = [ ".hs", ".hs-boot", ".cabal" ]

    guessExePath = do exePath <- getExecutablePath
                      return $ takeDirectory exePath </> "hfswatch"

-- | Stops the watch process and all threads associated with it.
stopWatch :: WatchProcess -> [ThreadId] -> IO ()
stopWatch WatchProcess{..} threads
  = do forM threads killThread
       wShutdown
