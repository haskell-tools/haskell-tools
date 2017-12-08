module Refactor.AutoCorrect.ThreeArgFirstReOrder where

x = f A B B

f :: A -> B -> B -> ()
f _ _ _ = ()

data A = A
data B = B