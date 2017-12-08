module Refactor.AutoCorrect.ThreeArgFirstReOrder where

x = f B A B

f :: A -> B -> B -> ()
f _ _ _ = ()

data A = A
data B = B