{-# LANGUAGE TupleSections #-}

module InRhsGuard where

f x
  | y1 <- (0,) x,   {-* TupleSections *-}
    y2 <- (0,) y1   {-* TupleSections *-}
  = ()
  | z1 <- (0,) x    {-* TupleSections *-}
  = ()
