{-# LANGUAGE TypeFamilies
           , GADTs
           #-}

module GDataFamilyDecl where

data family G a b {-* TypeFamilies *-}

data instance G [a] b where
   G1 :: c -> G [Int] b
   G2 :: G [a] Bool         {-* TypeFamilies *-}
