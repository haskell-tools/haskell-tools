module Refactor.ExtractBinding.Records where

data Point = Point { x :: Double, y :: Double }

d = (plus) (Point 1 2)
plus = \(Point {x = x, y = y}) -> x + y
