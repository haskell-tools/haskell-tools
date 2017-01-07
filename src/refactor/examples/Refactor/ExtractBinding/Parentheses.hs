module Refactor.ExtractBinding.Parentheses where

distance p1 p2 = sqrt ((x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2)

-- try to rename the type, the constructor or the fields
data Point = Point { x :: Double, y :: Double }