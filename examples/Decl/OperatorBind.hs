module Decl.OperatorBind where

a >< b = a ++ b

(<+>) :: Int -> Int -> Int -> Int
(a <+> b) c = a + b + c
