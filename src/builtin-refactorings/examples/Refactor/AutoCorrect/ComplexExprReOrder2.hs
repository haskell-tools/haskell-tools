module Refactor.AutoCorrect.ComplexExprReOrder2 where

x = f (b { b1 = 0 }) (A { a = "fdf" })

b = B 1 2

f :: A -> B -> ()
f _ _ = ()

data A = A { a :: String }
data B = B { b1 :: Int, b2 :: Int }