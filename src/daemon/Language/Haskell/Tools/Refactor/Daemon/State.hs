{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Refactor.Daemon.State where

import Control.Reference

import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Session

data DaemonSessionState 
  = DaemonSessionState { _refactorSession :: RefactorSessionState
                       , _exiting :: Bool
                       }

makeReferences ''DaemonSessionState

instance IsRefactSessionState DaemonSessionState where
  refSessMCs = refactorSession & refSessMCs
  initSession = DaemonSessionState initSession False