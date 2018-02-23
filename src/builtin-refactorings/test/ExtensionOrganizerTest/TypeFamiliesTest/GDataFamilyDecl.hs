{-# LANGUAGE TypeFamilies
           , GADTs
           #-}

module GDataFamilyDecl where

{-@ TypeFamilies, GADTs @-}

data family G a b {-* TypeFamilies *-}

data instance G [a] b where
   G1 :: c -> G [Int] b     {-* GADTSyntax, GADTs + ExistentialQuantification *-}
   G2 :: G [a] Bool         {-* TypeFamilies, GADTSyntax, GADTs + ExistentialQuantification *-}
