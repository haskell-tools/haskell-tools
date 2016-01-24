{-# LANGUAGE TypeFamilies #-}
module Decl.TypeClass where

class Show a => C a where
  type X a :: *
  type X a = Int
  data Q a :: *
  
  f :: a -> String
  f = show
  
