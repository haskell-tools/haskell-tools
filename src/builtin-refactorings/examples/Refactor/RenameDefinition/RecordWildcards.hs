{-# LANGUAGE RecordWildCards #-}
module Refactor.RenameDefinition.RecordWildcards where

data Point = Point { x :: Int, y :: Int }

p1 = let x = 3; y = 4 in Point { x = 1, .. }

p2 = let Point { .. } = Point 1 2 in y
