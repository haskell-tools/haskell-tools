module Decl.LocalBindings where

f x = g x
  where g :: Int -> Int
        g = id
        
f' x | even x = g x
  where g :: Int -> Int
        g = id