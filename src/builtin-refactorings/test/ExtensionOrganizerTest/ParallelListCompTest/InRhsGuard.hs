{-# LANGUAGE ParallelListComp #-}

module InRhsGuard where

f x
  | y1 <- [ (x,y) | x <- [1..10] | y <- [1..10] ],   {-* ParallelListComp *-}
    y2 <- [ (x,y) | x <- [1..10] | y <- [1..10] ]   {-* ParallelListComp *-}
  = ()
  | z1 <- [ (x,y) | x <- [1..10] | y <- [1..10] ]    {-* ParallelListComp *-}
  = ()
