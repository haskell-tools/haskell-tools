{-# LANGUAGE TypeFamilies #-}

module InTypeFamily where

type family F a :: * where  {-* KindSignatures *-}
  F () = ()
  F _  = ()                 {-* TypeFamilies *-}
