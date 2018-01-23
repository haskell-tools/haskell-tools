{-# LANGUAGE TypeFamilies #-}

module ClosedTypeFamilyDecl where

type family F a :: * where
  F Int  = Integer
  F Char = String  {-* TypeFamilies *-}
