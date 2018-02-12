{-# LANGUAGE MultiWayIf #-}

module InRhsGuard where

f x
  | y1 <- if | True -> 0 | False -> 1,   {-* MultiWayIf *-}
    y2 <- if | True -> 0 | False -> 1    {-* MultiWayIf *-}
  = ()
  | z1 <- if | True -> 0 | False -> 1    {-* MultiWayIf *-}
  = ()
