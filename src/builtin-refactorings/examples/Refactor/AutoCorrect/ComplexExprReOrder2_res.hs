module Refactor.AutoCorrect.ComplexExprReOrder2 where

x = f (A { a = "fdf" }) (b { b1 = 0 })

b = B 1 2

f :: A -> B -> ()
f _ _ = ()

data A = A { a :: String }
data B = B { b1 :: Int, b2 :: Int }