module Pattern.Backtick where

data Point = Point { x :: Prelude.Int, y :: Int }

f (x `Pattern.Backtick.Point` y) = 0
