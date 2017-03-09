{-# LANGUAGE ConstraintKinds #-}
module Type.TupleAssert where

f :: (Ord a, (Show a, Eq a)) => a -> String
f = show
