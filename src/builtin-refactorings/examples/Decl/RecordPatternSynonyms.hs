{-# LANGUAGE PatternSynonyms #-}
module Decl.RecordPatternSynonyms where

pattern XPoint {xp} <- (xp, 0) where XPoint xp = (xp, 0)

pattern Point {x, y} = (x, y)

r = (0, 0) { x = 1 }
