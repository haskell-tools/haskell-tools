{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module TH.ClassUse where

class C a where
  f :: a -> a

$([d|
    instance C a where
     f = id
 |])
