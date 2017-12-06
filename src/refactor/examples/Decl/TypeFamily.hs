{-# LANGUAGE TypeFamilies #-}
module Decl.TypeFamily where

type family Array a :: *

type instance Array () = Int
