{-# LANGUAGE TypeFamilies #-}

module TypeFamilyDecl where

type family F a :: * {-* TypeFamilies, KindSignatures *-}

type instance F Int  = Integer  {-* TypeFamilies *-}
type instance F Char = String   {-* TypeFamilies *-}
