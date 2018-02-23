{-# LANGUAGE GADTs #-}

module RecordContext where

{-@ GADTs @-}

data T a b where
  T1 :: Eq a => { f1::a, f2::b } -> T a b  {-* GADTSyntax, GADTs + ExistentialQuantification *-}
  T2 :: { f1::a, f2::b } -> T a b          {-* GADTSyntax *-}
