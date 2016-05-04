module Pattern.Backtick where

data Point = Point { x :: Int, y :: Int }

f (x `Point` y) = 0
