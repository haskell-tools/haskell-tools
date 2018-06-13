module Refactor.OrganizeImports.NarrowType where

import Control.Monad.State (State, StateT(..))

type St = State ()
type StT = StateT () IO

f = runStateT
