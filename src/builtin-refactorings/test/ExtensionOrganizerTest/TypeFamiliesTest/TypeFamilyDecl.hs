{-# LANGUAGE TypeFamilies #-}

module TypeFamilyDecl where

type family F a :: * {-* TypeFamilies *-}

type instance F Int  = Integer  {-* TypeFamilies *-}
type instance F Char = String   {-* TypeFamilies *-}
