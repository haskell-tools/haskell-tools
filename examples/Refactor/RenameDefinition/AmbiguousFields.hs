{-# LANGUAGE DuplicateRecordFields #-}
module Refactor.RenameDefinition.AmbiguousFields where

data A = A { x, y :: Int }
data B = B { x, y :: Int }

f :: A -> Int
f = x

g :: B -> Int
g = x
