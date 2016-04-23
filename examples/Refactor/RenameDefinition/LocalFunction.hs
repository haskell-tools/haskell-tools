module Refactor.RenameDefinition.LocalFunction where

f :: Int -> Int
f = x
  where x :: Int -> Int
        x = id


g :: Int -> Int
g = id 