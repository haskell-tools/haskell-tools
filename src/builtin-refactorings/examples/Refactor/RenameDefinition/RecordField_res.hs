module Refactor.RenameDefinition.RecordField where

data Point = Point { xCoord :: Double, y :: Double }

distance :: Point -> Point -> Double
distance p1 p2 = sqrt ((xCoord p1 - xCoord p2) ^ 2 + (y p1 - y p2) ^ 2)
