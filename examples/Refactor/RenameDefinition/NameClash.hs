module Refactor.RenameDefinition.NameClash where

f :: Int -> Int
f = g
  where g :: Int -> Int
        g = id