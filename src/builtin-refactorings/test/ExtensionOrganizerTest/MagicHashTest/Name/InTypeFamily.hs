{-# LANGUAGE MagicHash,
             TypeFamilyDependencies
             #-}

module InTypeFamily where

-- DeclHead, TypeEqn, InjectivityAnn

-- a function on types: gets a type 'a' and return a type with kind *
type family Id# a# = r# | r# -> a# where  {-* MagicHash, MagicHash, MagicHash, MagicHash, MagicHash*-}  --TypeFamilies, TypeFamilyDependencies
  Id# a# = a#  {-* MagicHash, MagicHash *-}
