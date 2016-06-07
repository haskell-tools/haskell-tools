{-# LANGUAGE DuplicateRecordFields #-}
module Decl.AmbiguousFields where

data A = A { x, y :: Int }
data B = B { x, y :: Int }

f :: A -> Int
f = x

