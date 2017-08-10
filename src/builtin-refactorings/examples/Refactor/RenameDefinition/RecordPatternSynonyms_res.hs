{-# LANGUAGE PatternSynonyms #-}
module Refactor.RenameDefinition.RecordPatternSynonyms where

pattern Point {xx, y} = (xx, y)

r = (0, 0) { xx = 1 }