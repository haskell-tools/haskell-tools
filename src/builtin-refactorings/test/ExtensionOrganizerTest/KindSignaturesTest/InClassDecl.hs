{-# LANGUAGE KindSignatures #-}

module InClassDecl where

class C (a :: *) where {-* KindSignatures *-}
  f :: a -> ()
