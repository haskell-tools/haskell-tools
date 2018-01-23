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

type HiddenEqRel a b = EqRel a b {-* TypeFamilies *-}

type ComplexEqRelType a =
  Eq a => a -> a -> (forall c . HiddenEqRel a c => c -> c -> Bool) -> Bool  {-* TypeFamilies *-}
