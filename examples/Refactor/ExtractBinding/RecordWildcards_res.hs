{-# LANGUAGE RecordWildCards #-}
module Refactor.ExtractBinding.RecordWildcards where

data Point = Point { x :: Double, y :: Double }

d = plus (Point 1 2)
  where plus (Point {..}) = x + y
