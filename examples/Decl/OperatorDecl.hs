module Decl.OperatorDecl where

(-!-) :: Int -> Int -> Int
(-!-) a b = a + b

test = (-!-) (1 -!- 2) 3