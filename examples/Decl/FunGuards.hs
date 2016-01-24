module Decl.FunGuards where

f 0 = 1
f x | even x = 0
    | otherwise = 2