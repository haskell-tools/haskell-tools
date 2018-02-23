{-# LANGUAGE TypeFamilies
           , GADTs
           #-}

module GDataFamilyDecl where

data family G a b {-* TypeFamilies *-}

data instance G [a] b where
   G1 :: c -> G [Int] b     {-* GADTSyntax, GADTs + ExistentialQuantification *-}
   G2 :: G [a] Bool         {-* GADTSyntax, GADTs + ExistentialQuantification, TypeFamilies *-}
