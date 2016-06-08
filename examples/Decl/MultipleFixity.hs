module Decl.MultipleFixity where

f :: Int -> Int -> Int
f a b = a + b

g = f

infixl 6 `f`, `g`

h = f

infixl 5 `h`