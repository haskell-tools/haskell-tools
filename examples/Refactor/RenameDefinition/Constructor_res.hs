module Refactor.RenameDefinition.Constructor where

data Point = Point2D Double Double

distance :: Point -> Point -> Double
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
