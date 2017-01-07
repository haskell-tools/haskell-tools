module Refactor.InlineBinding.FilterSignatures where

b u v = u <++> v
(<++>), (<**>) :: String -> String -> String
x <++> y = x ++ y
x <**> y = y ++ x
infixl 7 <++>, <**>