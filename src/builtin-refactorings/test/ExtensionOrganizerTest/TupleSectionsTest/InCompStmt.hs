{-# LANGUAGE TupleSections #-}

module InCompStmt where

xs = [ (0,) x | x <- [1..10] ] {-* TupleSections *-}

ys = [ fst y | y <- [(0,) 1] ]  {-* TupleSections *-}

zs = [ (0,) z | z <- [(0,) 1] ]  {-* TupleSections, TupleSections *-}
