module Refactor.AutoCorrect.ComplexExprReOrder where

x = f (B 1 2) (A "A")

f :: A -> B -> ()
f _ _ = ()

data A = A String
data B = B Int Int