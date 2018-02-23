{-# LANGUAGE GADTSyntax #-}

module NormalOnlySyntax where

data T a where
  T1 :: a -> T a       {-* GADTSyntax *-}
  T2 :: a -> a -> T a  {-* GADTSyntax *-}
