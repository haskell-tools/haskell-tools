{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Daemon.State where

import Control.Concurrent
import Control.Reference
import GHC
import System.FSWatch.Repr

import Language.Haskell.Tools.Daemon.PackageDB
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Refactor

-- | The actual state of the daemon process. Contains loaded modules and user settings.
-- The GHC state is handled separately.
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
                       , _undoStack :: [[UndoRefactor]]
                           -- ^ True if in the process of shutting down the session.
                       , _watchProc :: Maybe WatchProcess
                           -- ^ Information about the file system watch process.
                       , _watchThreads :: [ThreadId]
                           -- ^ Extra threads started for handling the
                           -- information from the watch process.
                       }

-- | An initial state of a daemon session.
initSession :: DaemonSessionState
initSession = DaemonSessionState [] AutoDB id False [] False [] Nothing []

makeReferences ''DaemonSessionState
