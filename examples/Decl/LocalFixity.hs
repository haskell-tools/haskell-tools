module Decl.LocalFixity where

x = 1 `f` 2
  where f :: Int -> Int -> Int
        f a b = a + b 
        infixl 6 `f`
