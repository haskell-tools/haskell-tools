{-# LANGUAGE MultiWayIf,
             ViewPatterns
             #-}

module InPattern where

f ((if | True -> id | False -> id) -> []) = ()   {-* MultiWayIf, ViewPatterns *-}
f ((if | True -> id | False -> id) -> ys) = ()   {-* MultiWayIf, ViewPatterns *-}
