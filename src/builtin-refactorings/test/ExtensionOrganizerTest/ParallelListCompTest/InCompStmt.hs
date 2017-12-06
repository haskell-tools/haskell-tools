{-# LANGUAGE ParallelListComp #-}

module InCompStmt where

{-# ANN module "HLint: ignore Redundant list comprehension" #-}

xs = [ [ (x,y) | x <- [1..10] | y <- [1..10] ] | z <- [1..10] ] {-* ParallelListComp *-}

ys = [ z | z <- [ (x,y) | x <- [1..10] | y <- [1..10] ] ]  {-* ParallelListComp *-}

zs = [ [ (x,y) | x <- [1..10] | y <- [1..10] ] | z <- [ (x,y) | x <- [1..10] | y <- [1..10] ] ]  {-* ParallelListComp, ParallelListComp *-}
