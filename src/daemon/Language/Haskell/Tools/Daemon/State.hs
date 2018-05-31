{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Tools.Daemon.State where

import Control.Concurrent
import Control.Reference
import Data.Set
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
                       , _packageDB :: Maybe (PackageDB, Bool)
                           -- ^ The package database that is selected and a flag to decide if it is forced.
                       , _ghcFlagsSet :: DynFlags -> DynFlags
                           -- ^ GHC flags for compiling modules. Overrides settings in cabal files.
                       , _pkgDbFlags :: DynFlags -> DynFlags
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
                       , _touchedFiles :: Set FilePath
                           -- ^ Marks files as being modified by this process
                           -- Changes detected on marked files will not invalidate refactoring history.
                       }

-- | An initial state of a daemon session.
initSession :: DaemonSessionState
initSession = DaemonSessionState [] Nothing id id [] False [] Nothing [] empty

resetSession :: DaemonSessionState -> DaemonSessionState
resetSession DaemonSessionState{..} = initSession { _packageDB, _ghcFlagsSet }

makeReferences ''DaemonSessionState
