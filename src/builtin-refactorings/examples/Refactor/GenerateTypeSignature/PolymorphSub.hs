module Refactor.GenerateTypeSignature.PolymorphSub where

f :: Num a => a -> a
f a = g a where
  g a = a + 1
