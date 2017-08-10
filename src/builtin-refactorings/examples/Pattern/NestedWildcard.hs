{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Pattern.NestedWildcard where

data A = A { b :: B, ai :: Int }
data B = B { bi :: Int }

h A { b = B {..}, .. } = bi + ai
