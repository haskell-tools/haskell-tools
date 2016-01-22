{-# LANGUAGE RecordWildCards #-}
module Expr.RecordWildcards where

data Point = Point { x :: Int, y :: Int }

p1 = let x = 3; y = 4 in Point { x = 1, .. }