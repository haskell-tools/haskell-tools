{-# LANGUAGE TypeFamilies #-}

module ClosedTypeFamilyDecl where

type family F a :: * where  {-* KindSignatures *-}
  F Int  = Integer
  F Char = String  {-* TypeFamilies *-}
