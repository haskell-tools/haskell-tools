{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.UseImported where

data T a = T a

$([d|
  instance Show a => Show (T a) where show = undefined
  instance (Eq a, Show a) => Eq (T a) where (==) = undefined
  |])