{-# LANGUAGE TypeFamilies #-}
module Package.ClosedTypeFamily where

type family F a where
  F Int  = Bool
  F Bool = Char
  F a    = Bool