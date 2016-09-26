{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Refactor.Session where

import Data.Map as Map
import Control.Monad.State
import Control.Reference

import Language.Haskell.Tools.AST (IdDom)
import Language.Haskell.Tools.Refactor (IsBoot(..))
import Language.Haskell.Tools.Refactor.RefactorBase (UnnamedModule)

type RefactorSession = StateT RefactorSessionState

data RefactorSessionState
  = RefactorSessionState { _refSessMods :: Map.Map (String, String, IsBoot) (UnnamedModule IdDom)
                         , _actualMod :: Maybe (String, String, IsBoot)
                         , _exiting :: Bool
                         , _dryMode :: Bool
                         }

initSession :: RefactorSessionState
initSession = RefactorSessionState Map.empty Nothing False False

makeReferences ''RefactorSessionState