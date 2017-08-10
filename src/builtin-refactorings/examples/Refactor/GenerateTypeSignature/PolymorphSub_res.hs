module Refactor.GenerateTypeSignature.PolymorphSub where

f :: Num a => a -> a
f a = g a where
  g :: Num a => a -> a
  g a = a + 1