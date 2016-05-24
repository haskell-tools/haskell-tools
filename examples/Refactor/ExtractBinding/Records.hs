module Refactor.ExtractBinding.Records where

data Point = Point { x :: Double, y :: Double }

d = (\(Point {x = x, y = y}) -> x + y) (Point 1 2)
