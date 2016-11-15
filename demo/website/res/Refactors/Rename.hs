module Refactors.Rename where

-- TODO: use [Ctrl, Alt and/or Shift + R] to rename the selected definition
-- The function f calculates the average of two numbers. Rename it to 'avg' for clarity.
-- Bonus: also rename the type variable.
f :: (Num a, Integral a) => a -> a -> a
f a b = (a + b) `div` 2

-- TODO: This datatype should be called Point2D to be precise.
-- You should also rename the fields to avoid one letter field names.
data Point = Point { x :: Int, y :: Int }

-- TODO: how should we call g?
g :: Point -> Point -> Point
g p1 p2 = Point (f (x p1) (x p2)) (f (y p1) (y p2))
