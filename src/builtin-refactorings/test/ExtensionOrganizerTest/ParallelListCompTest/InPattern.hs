{-# LANGUAGE ParallelListComp,
             ViewPatterns
             #-}

module InPattern where

f (const [ (x,y) | x <- [1..10] | y <- [1..10] ] -> []) = ()   {-* ParallelListComp, ViewPatterns *-}
f (const [ (x,y) | x <- [1..10] | y <- [1..10] ] -> ys) = ()   {-* ParallelListComp, ViewPatterns *-}
