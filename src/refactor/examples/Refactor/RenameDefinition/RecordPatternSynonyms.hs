{-# LANGUAGE PatternSynonyms #-}
module Refactor.RenameDefinition.RecordPatternSynonyms where

pattern Point {x, y} = (x, y)

r = (0, 0) { x = 1 }