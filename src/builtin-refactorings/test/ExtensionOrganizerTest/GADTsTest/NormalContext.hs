{-# LANGUAGE GADTs #-}

module NormalContext where

{-@ GADTs @-}

data T a where
  T1 :: Eq a => a -> T a  {-* GADTSyntax, GADTs + ExistentialQuantification *-}
  T2 :: a -> a -> T a     {-* GADTSyntax *-}
