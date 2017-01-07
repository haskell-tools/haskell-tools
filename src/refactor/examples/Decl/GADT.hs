{-# LANGUAGE GADTs, KindSignatures #-}
module Decl.GADT where

data G2 a :: * where
  G2A :: { g2a :: a, g2b :: Int } -> G2 a