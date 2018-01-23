{-# LANGUAGE TypeFamilies #-}

module DataFamilyDecl where

data family F a :: * {-* TypeFamilies *-}

data instance F Int  = F1  {-* TypeFamilies *-}
data instance F Char = F2  {-* TypeFamilies *-}
