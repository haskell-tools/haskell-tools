{-# LANGUAGE BangPatterns #-}

module InStmt where

f = do
  let !x = 5              {-* BangPatterns *-}
  (!a,_) <- Just (1,2)    {-* BangPatterns *-}
  return a
