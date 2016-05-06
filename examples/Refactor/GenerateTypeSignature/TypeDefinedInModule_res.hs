module Refactor.GenerateTypeSignature.TypeDefinedInModule where

distance :: Point -> Point -> Double
distance p1 p2 = sqrt ((x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2)

data Point = Point { x :: Double, y :: Double }

