{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Pattern.Record where

data Point = Point { x :: Int, y :: Int }

f Point { x = 3, y = 1 } = 0
f Point {} = 1

g Point { x } = x

h Point { x = 1, .. } = y
