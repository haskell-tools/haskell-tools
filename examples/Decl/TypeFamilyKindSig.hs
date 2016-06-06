{-# LANGUAGE TypeFamilies #-}
module Decl.TypeFamilyKindSig where

type family Array (a :: *) :: *

type instance Array () = Int
