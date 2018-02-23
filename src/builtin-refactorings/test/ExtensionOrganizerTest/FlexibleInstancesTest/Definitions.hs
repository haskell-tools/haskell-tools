{-# LANGUAGE MultiParamTypeClasses,
             TypeOperators
             #-}

module Definitions where

class C1 a where
  f1 :: a -> Bool

class C2 a b where
  f2 :: a -> b -> Bool

class a :?: b where
  h :: a -> b -> Bool

class a :!: b where
  j :: a -> b -> Bool


data T4 a b c d = T4 a b c d
data T3 a b c = T3 a b c
data T2 a b = T2 a b
data T1 a = T1 a
data T0 = T0

type TS4 a b c d = T4 a b c d
type TS3 a b c = T3 a b c
type TS2 a b = T2 a b
type TS1 a = T1 a
type TS0 = T0

data a :-: b      = Minus a b
data a :+: b      = Plus a b
data (a :++: b) c = PPlus a b c

type MinusSyn a b   = a :-: b
type PlusSyn  a b   = a :+: b
type PPlusSyn a b c = (a :++: b) c


type Phantom    a b = [a]
type HomoTuple  a   = (a,a)
type DoubleList a   = [[a]]
