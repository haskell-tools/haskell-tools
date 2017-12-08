module Refactor.AutoCorrect.ComplexExprReOrder where

x = f (A "A") (B 1 2)

f :: A -> B -> ()
f _ _ = ()

data A = A String
data B = B Int Int