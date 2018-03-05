{-# LANGUAGE GADTs #-}

module NormalSpecResultType where

{-@ GADTs @-}

data T a where
  T1 :: a -> T [a]       {-* GADTSyntax, GADTs + ExistentialQuantification *-}
  T2 :: a -> a -> T a    {-* GADTSyntax *-}
