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

data DaemonSessionState
  = DaemonSessionState { _refactorSession :: RefactorSessionState
                       , _packageDB :: PackageDB
                       , _ghcFlagsSet :: DynFlags -> DynFlags
                       , _packageDBSet :: Bool
                       , _packageDBLocs :: [FilePath]
                       , _exiting :: Bool
                       , _watchProc :: Maybe WatchProcess
                       }

data WatchProcess
  = WatchProcess { _watchPHandle :: ProcessHandle
                 , _watchStdIn :: Handle
                 , _watchStdOut :: Handle
                 , _watchThreads :: [ThreadId]
                 }


-- | The state common for refactoring tools, carrying the state of modules.
data RefactorSessionState
  = RefactorSessionState { __refSessMCs :: [ModuleCollection SourceFileKey]
                         }

makeReferences ''RefactorSessionState
makeReferences ''DaemonSessionState

instance IsRefactSessionState RefactorSessionState where
  refSessMCs = _refSessMCs
  initSession = RefactorSessionState []

instance IsRefactSessionState DaemonSessionState where
  refSessMCs = refactorSession & refSessMCs
  initSession = DaemonSessionState initSession AutoDB id False [] False Nothing
