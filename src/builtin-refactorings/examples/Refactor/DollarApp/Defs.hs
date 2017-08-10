module Refactor.DollarApp.Defs where

f = id
g = id

($$) :: (a -> b) -> a -> a
f $$ a = a

infixl 0 $$