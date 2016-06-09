module Decl.InstanceSpec where

data Foo a = Foo a

instance (Eq a) => Eq (Foo a) where 
   {-# SPECIALIZE instance Eq (Foo Char) #-}
   Foo a == Foo b = a == b