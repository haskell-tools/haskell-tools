{-# LANGUAGE PatternSynonyms #-}
module Decl.RecordPatternSynonyms where

pattern Point {x, y} = (x, y)

r = (0, 0) { x = 1 }