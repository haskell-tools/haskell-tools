module Refactor.FloatOut.MoveFixity where

f = 3 <+> 4
  where infixl 6 <+>
        a <+> b = a + b