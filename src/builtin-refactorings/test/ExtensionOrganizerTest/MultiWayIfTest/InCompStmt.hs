{-# LANGUAGE MultiWayIf #-}

module InCompStmt where

xs = [ (if | x == 0 -> 0 | x == 1 -> 1) | x <- [1..10] ] {-* MultiWayIf *-}

ys = [ 2*y | y <- (if | 0 == 0 -> [] | 1 == 1 -> []) ]  {-* MultiWayIf *-}
