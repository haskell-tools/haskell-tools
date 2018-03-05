{-# LANGUAGE MultiWayIf #-}

module InCaseRhs where

f x = case x of
        [] -> if | True -> 0 | False -> 1    {-* MultiWayIf *-}
        xs -> if | True -> 0 | False -> 1    {-* MultiWayIf *-}
