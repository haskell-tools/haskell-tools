{-# LANGUAGE GADTs, KindSignatures, TypeFamilies, TemplateHaskell #-}
module TH.GADTFields where

data Gadtrec1 a where
  Gadtrecc1, Gadtrecc2 :: { gadtrec1a :: a, gadtrec1b :: b } -> Gadtrec1 (a,b)

$( let a = ['gadtrec1a, 'gadtrec1b] in return [] )
