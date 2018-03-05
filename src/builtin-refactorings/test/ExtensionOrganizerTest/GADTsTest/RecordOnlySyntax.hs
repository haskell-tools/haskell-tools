{-# LANGUAGE GADTSyntax #-}

module RecordOnlySyntax where

data T a b where
  T1 :: { f1::a, f2::b } -> T a b  {-* GADTSyntax *-}
  T2 :: { f1::a, f2::b } -> T a b  {-* GADTSyntax *-}
