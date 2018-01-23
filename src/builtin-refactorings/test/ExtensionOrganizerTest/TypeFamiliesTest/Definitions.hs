{-# LANGUAGE TypeFamilies
           , ConstraintKinds
           , RankNTypes
            #-}

module Definitions where

type EqRel a b = a ~ b {-* TypeFamilies *-}

type TrfAB a b = a ~ b => a -> b  {-* TypeFamilies *-}

type family F a :: * where
  F Int  = Integer
  F Char = String
  F a    = a  {-* TypeFamilies *-}
