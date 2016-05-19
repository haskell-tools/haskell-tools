{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Decl.FunctionalDeps where

class C a b | a -> b, b -> a where
  trf :: a -> b
  
