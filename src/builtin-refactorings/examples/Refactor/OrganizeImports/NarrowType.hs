module Refactor.OrganizeImports.NarrowType where

import Control.Monad.State

type St = State ()
type StT = StateT () IO

f = runStateT
