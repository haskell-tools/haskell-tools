{-# LANGUAGE TypeFamilies, InstanceSigs #-}
module Decl.TypeInstance where

import Decl.TypeClass

data A = A deriving Show

instance C A where
  type X A = Int
  data Q A = Bool

  f :: A -> String
  f A = "XXX"
  
