module Refactor.RenameDefinition.CrossRename where

-- Renaming f to g should fail
f x = x

ff x = g
  where
    g = f 'g'
