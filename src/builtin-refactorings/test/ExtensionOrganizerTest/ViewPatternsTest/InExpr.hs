{-# LANGUAGE ViewPatterns #-}

module InExpr where

{-# ANN module "HLint: ignore Redundant lambda" #-}

f g = let (g -> (a,b)) = (0,0) in a + b  {-* ViewPatterns *-}

h j = \(j -> (x,y)) -> x + y {-* ViewPatterns *-}
