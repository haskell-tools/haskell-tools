{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}

module Definitions where

data T a = T a

class C a where
  foo :: a -> ()

class a :?: b where
  q :: a -> b -> ()
