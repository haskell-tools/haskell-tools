module Refactor.InlineBinding.RemoveSignatures where

b u v = u <++> v
(<++>) :: String -> String -> String
x <++> y = x ++ y
infixl 7 <++>