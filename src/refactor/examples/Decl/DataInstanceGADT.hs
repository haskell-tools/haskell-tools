{-# LANGUAGE GADTs, KindSignatures, TypeFamilies #-}
module Decl.DataInstanceGADT where

data family GadtFam (a :: *) (b :: *)
data instance GadtFam c d where
  MkGadtFam1 :: x -> y -> GadtFam y x
