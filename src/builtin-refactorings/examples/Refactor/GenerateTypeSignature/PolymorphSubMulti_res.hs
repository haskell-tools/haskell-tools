module Refactor.GenerateTypeSignature.PolymorphSubMulti where

f :: (Num a, Ord a) => a -> a
f a = g a where
  g :: (Num p, Ord p) => p -> p
  g a = if a > 0 then a + 1 else a
