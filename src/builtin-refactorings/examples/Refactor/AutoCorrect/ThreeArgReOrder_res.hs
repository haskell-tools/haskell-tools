module Refactor.AutoCorrect.ThreeArgReOrder where

x = f B B A

f :: B -> B -> A -> ()
f _ _ _ = ()

data A = A
data B = B