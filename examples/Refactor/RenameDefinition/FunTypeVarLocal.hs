module Refactor.RenameDefinition.FunTypeVarLocal where

g = f () 
  where
    f :: a -> a
    f x = x