{-# LANGUAGE ParallelListComp #-}

module InCaseRhs where

f x = case x of
        [] -> [ (x,y) | x <- [1..10] | y <- [1..10] ]    {-* ParallelListComp *-}
        xs -> [ (x,y) | x <- [1..10] | y <- [1..10] ]    {-* ParallelListComp *-}
