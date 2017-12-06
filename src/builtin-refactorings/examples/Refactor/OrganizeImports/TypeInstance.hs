{-# LANGUAGE TypeFamilies, InstanceSigs #-}
module Refactor.OrganizeImports.TypeInstance where

import Refactor.OrganizeImports.TypeClass

data A = A deriving Show

instance C A where
  type X A = Int
  data Q A = Bool

  f :: A -> String
  f A = "XXX"
  
