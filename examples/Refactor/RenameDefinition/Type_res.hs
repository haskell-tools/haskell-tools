module Refactor.RenameDefinition.Type where

data Point2D = Point Double Double

distance :: Point2D -> Point2D -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
