module Pattern.Backtick where

data Point = Point { x :: Prelude.Int, y :: Int }

f (x `Point` y) = 0
