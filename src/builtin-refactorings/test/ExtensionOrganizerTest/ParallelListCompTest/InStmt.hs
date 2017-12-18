{-# LANGUAGE ParallelListComp #-}

module InStmt where

f x = do
  let k = [ (x,y) | x <- [1..10] | y <- [1..10] ]     {-* ParallelListComp *-}
  Just [ (x,y) | x <- [1..10] | y <- [1..10] ]        {-* ParallelListComp *-}
  y <- Just [ (x,y) | x <- [1..10] | y <- [1..10] ]   {-* ParallelListComp *-}
  z <- Just [ (x,y) | x <- [1..10] | y <- [1..10] ]   {-* ParallelListComp *-}
  return $ [ (x,y) | x <- [1..10] | y <- [1..10] ]    {-* ParallelListComp *-}
