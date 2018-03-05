{-# LANGUAGE GADTs #-}

module RecordExistential where

{-@ GADTs @-}

data T a b where
  T1 :: { f1::a, f2::c } -> T a b  {-* GADTSyntax, GADTs + ExistentialQuantification *-}
  T2 :: { g1::a, g2::b } -> T a b  {-* GADTSyntax *-}
