{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
module Decl.InjectiveTypeFamily where

type family Array a = r | r -> a

type instance Array () = Int
