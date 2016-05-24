{-# LANGUAGE RecordWildCards #-}
module Refactor.ExtractBinding.RecordWildcards where

data Point = Point { x :: Double, y :: Double }

d = (\(Point {..}) -> x + y) (Point 1 2)
