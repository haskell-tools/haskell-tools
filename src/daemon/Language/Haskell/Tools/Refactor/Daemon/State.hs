{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Refactor.Daemon.State where

import Control.Reference
import Control.Concurrent
import System.Process
import System.IO

import Language.Haskell.Tools.Refactor.Daemon.PackageDB
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session

data DaemonSessionState
  = DaemonSessionState { _refactorSession :: RefactorSessionState
                       , _packageDB :: PackageDB
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


makeReferences ''DaemonSessionState
makeReferences ''WatchProcess

instance IsRefactSessionState DaemonSessionState where
  refSessMCs = refactorSession & refSessMCs
  initSession = DaemonSessionState initSession AutoDB False [] False Nothing
