{-# LANGUAGE TypeOperators #-}
module Refactor.RenameDefinition.WrongName where

f :: Int -> Int
f x = x

data X = X

data a .+. b = a :+: b

(+++) = (+)