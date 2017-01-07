{-# LANGUAGE RecordWildCards #-}
module Refactor.RenameDefinition.RecordWildcards where

data Point = Point { x :: Int, yy :: Int }

p1 = let x = 3; yy = 4 in Point { x = 1, .. }

p2 = let Point { .. } = Point 1 2 in yy
