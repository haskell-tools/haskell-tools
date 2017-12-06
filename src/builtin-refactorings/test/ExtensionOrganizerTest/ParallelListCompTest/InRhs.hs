{-# LANGUAGE ParallelListComp #-}

module InRhs where

f [] = [ (x,y) | x <- [1..10] | y <- [1..10] ]    {-* ParallelListComp *-}
f x  = [ (x,y) | x <- [1..10] | y <- [1..10] ]    {-* ParallelListComp *-}
