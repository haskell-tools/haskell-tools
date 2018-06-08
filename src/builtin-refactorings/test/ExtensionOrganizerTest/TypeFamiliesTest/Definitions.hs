{-# LANGUAGE TypeFamilies
           , ConstraintKinds
           , RankNTypes
            #-}

module Definitions where

type EqRel a b = a ~ b

type TrfAB a b = a ~ b => a -> b

type family F a :: * where
  F Int  = Integer
  F Char = String
  F a    = a

type HiddenEqRel a b = EqRel a b

type ComplexEqRelType a =
  Eq a => a -> a -> (forall c . HiddenEqRel a c => c -> c -> Bool) -> Bool


eqRelName :: EqRel a b => a -> b
eqRelName = id

nestedEqRelName :: a -> (forall a b . EqRel a b => a -> b) -> a
nestedEqRelName x f = f x
