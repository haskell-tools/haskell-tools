module Refactor.RenameDefinition.LocalFunction where

f :: Int -> Int
f = g
  where g :: Int -> Int
        g = id


g :: Int -> Int
g = id 