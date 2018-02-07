{-# LANGUAGE GADTs #-}

module NormalExistential where

{-@ GADTs @-}

data T a where
  T1 :: b -> T a          {-* GADTSyntax, GADTs + ExistentialQuantification *-}
  T2 :: a -> a -> T a     {-* GADTSyntax *-}
