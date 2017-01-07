module Refactor.RenameDefinition.FunTypeVarLocal where

g = f () 
  where
    f :: b -> b
    f x = x