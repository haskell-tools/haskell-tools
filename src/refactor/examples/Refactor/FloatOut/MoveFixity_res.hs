module Refactor.FloatOut.MoveFixity where

f = 3 <+> 4
  
infixl 6 <+>
a <+> b = a + b