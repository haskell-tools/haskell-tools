{-# LANGUAGE TypeFamilies #-}
module Decl.ClosedTypeFamily where

type family F a where
  F Int  = Bool
  F Bool = Char
  F a    = Bool

type family ClosedEmpty t where
