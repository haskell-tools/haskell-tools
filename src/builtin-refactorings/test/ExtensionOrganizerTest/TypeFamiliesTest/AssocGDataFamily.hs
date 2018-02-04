{-# LANGUAGE TypeFamilies, GADTs #-}

module AssocGDataFamily where

class GC a where
  data GD a :: * -> *           {-* TypeFamilies, KindSignatures, KindSignatures, KindSignatures *-}

instance GC Int where
  data GD Int a where
    G1 :: Int -> a -> GD Int a  {-* TypeFamilies *-}
