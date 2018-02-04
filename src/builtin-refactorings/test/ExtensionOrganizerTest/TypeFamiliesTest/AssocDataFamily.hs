{-# LANGUAGE TypeFamilies #-}

module AssocDataFamily where

class GMapKey k where
  data GMap k :: * -> *  {-* TypeFamilies, KindSignatures, KindSignatures, KindSignatures *-}

instance GMapKey Int where
  data GMap Int [v]   = G1 v  {-* TypeFamilies *-}
  data GMap Int (a,a) = G2 a  {-* TypeFamilies *-}
