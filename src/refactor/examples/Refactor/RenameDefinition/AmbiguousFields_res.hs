{-# LANGUAGE DuplicateRecordFields #-}
module Refactor.RenameDefinition.AmbiguousFields where

data A = A { xx, y :: Int }
data B = B { x, y :: Int }

f :: A -> Int
f = xx

g :: B -> Int
g = x
