{-# LANGUAGE ParallelListComp,
             TupleSections
             #-}

module InTupSecElem where

f = (,[ (x,y) | x <- [1..10] | y <- [1..10] ]) {-* ParallelListComp, TupleSections *-}
