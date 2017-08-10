module Refactor.OrganizeImports.InstanceCarry.TCWithInst where

import Refactor.OrganizeImports.InstanceCarry.DataType

class D t where
  g :: t -> t

instance D A where
  g = id
