{-# LANGUAGE BangPatterns #-}

module InExpr where

{-# ANN module "HLint: ignore Redundant lambda" #-}

x = let (!a,!b) = (0,0) in a + b  {-* BangPatterns, BangPatterns *-}

f = \(!x,y) -> x + y {-* BangPatterns *-}
