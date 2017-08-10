{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Daemon.State where

import Control.Concurrent
import Control.Reference
import System.IO
import System.Process

import Language.Haskell.Tools.Daemon.PackageDB
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.Session

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

instance IsRefactSessionState DaemonSessionState where
  refSessMCs = refactorSession & refSessMCs
  initSession = DaemonSessionState initSession AutoDB False [] False Nothing
