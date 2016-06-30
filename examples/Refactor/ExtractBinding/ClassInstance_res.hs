module Refactor.ExtractBinding.ClassInstance where

data Better a = Better a

instance Functor Better where
  fmap f (Better a) = Better g
    where g = f a
