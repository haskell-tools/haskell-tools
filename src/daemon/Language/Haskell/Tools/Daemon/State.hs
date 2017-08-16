{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Daemon.State where

import Control.Concurrent
import Control.Reference
import System.IO
import System.Process
import GHC

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Daemon.PackageDB
import Language.Haskell.Tools.Daemon.Representation

-- | A state for carrying sessions in the daemon
data DaemonSessionState
  = DaemonSessionState { _refSessMCs :: [ModuleCollection SourceFileKey]
                           -- ^ The package components loaded into the session.
                       , _packageDB :: PackageDB
                           -- ^ The package database that is selected.
                       , _ghcFlagsSet :: DynFlags -> DynFlags
                           -- ^ GHC flags for compiling modules. Overrides settings in cabal files.
                       , _packageDBSet :: Bool
                           -- ^ True if the package database is actually used. The package database
                           -- cannot be changed if set.
                       , _packageDBLocs :: [FilePath]
                           -- ^ The pathes where the package databases are located.
                       , _exiting :: Bool
                           -- ^ True if in the process of shutting down the session.
                       , _watchProc :: Maybe WatchProcess
                           -- ^ Information about the file system watch process.
                       }

-- | An initial state of a daemon session.
initSession :: DaemonSessionState
initSession = DaemonSessionState [] AutoDB id False [] False Nothing

-- | The state of the file system watching. Needed for adding new packages and shutting it down when
-- we are finished.
data WatchProcess
  = WatchProcess { _watchPHandle :: ProcessHandle -- ^ The handle for the watch process.
                 , _watchStdIn :: Handle -- ^ Input of the watch process.
                 , _watchStdOut :: Handle -- ^ Output of the watch process.
                 , _watchThreads :: [ThreadId] -- ^ Extra threads started for handling the
                                               -- information from the watch process.
                 }

makeReferences ''DaemonSessionState
