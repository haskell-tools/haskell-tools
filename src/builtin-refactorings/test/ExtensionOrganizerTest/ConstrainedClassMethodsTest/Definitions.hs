{-# LANGUAGE MultiParamTypeClasses #-}

module Definitions where

class C0 where
  f :: ()

class D a where
  g :: a -> ()
