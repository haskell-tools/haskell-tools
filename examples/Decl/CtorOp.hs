{-# LANGUAGE TypeOperators #-}
module Decl.CtorOp where

data a :+: b = a :+: b

data (a :!: b) c = a c :!: b c

data ((:-:) a) b = a :-: b

data (:*:) a b = a :*: b
