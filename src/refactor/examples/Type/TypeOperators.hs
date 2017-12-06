{-# LANGUAGE TypeOperators #-}
module Type.TypeOperators where

infixr 6 :+:
data a :+: r = a :+: r 

type X = Int :+: Char :+: String

type (:&:) = (,)
infixr :&: