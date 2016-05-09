module Refactor.ExtractBinding.ClassInstance where

data Better a = Better a

instance Functor Better where
  fmap f (Better a) = Better (g f a)
g f a = f a
