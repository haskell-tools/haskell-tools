{-# LANGUAGE MultiWayIf #-}

module InRhs where

f [] = if | True -> 0 | False -> 1    {-* MultiWayIf *-}
f x  = if | True -> 0 | False -> 1    {-* MultiWayIf *-}
