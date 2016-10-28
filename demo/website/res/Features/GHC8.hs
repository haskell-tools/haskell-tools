-- In GHC 8.0, the DuplicateRecordFields extension adds the possibility
-- to define records that have tha same name.

{-# LANGUAGE DuplicateRecordFields #-}
module Features.GHC8 where

-- TODO: rename one field with [Ctrl, UAlt and/or Shift + R]
-- notice that only the corresponding field selector is changed.
data A = A { x, y :: Int }
data B = B { x, y :: Int }

f :: A -> Int
f = x

g :: B -> Int
g = x
