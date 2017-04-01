{-# LANGUAGE TypeOperators, TypeFamilies #-}
module Decl.InfixInstances where

data Zipper h i a = Zipper
data (:@) a i
infixl 8 :>
type family (:>) h p
type instance h :> (a :@ i) = Zipper h i a
