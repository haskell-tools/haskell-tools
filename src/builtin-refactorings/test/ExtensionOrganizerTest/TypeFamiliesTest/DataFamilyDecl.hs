{-# LANGUAGE TypeFamilies #-}

module DataFamilyDecl where

data family F a :: * {-* TypeFamilies, KindSignatures *-}

data instance F Int  = F1  {-* TypeFamilies *-}
data instance F Char = F2  {-* TypeFamilies *-}
