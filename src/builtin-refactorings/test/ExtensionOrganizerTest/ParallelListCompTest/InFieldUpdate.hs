{-# LANGUAGE ParallelListComp #-}

module InFieldUpdate where

import Definitions

f Rec { num = num } = Rec {num = length ([ (x,y) | x <- [1..10] | y <- [1..10] ])} {-* ParallelListComp *-}
