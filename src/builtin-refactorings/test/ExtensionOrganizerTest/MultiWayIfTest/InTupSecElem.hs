{-# LANGUAGE MultiWayIf,
             TupleSections
             #-}

module InTupSecElem where

f = (,if | True -> 0 | False -> 1) {-* MultiWayIf, TupleSections *-}
