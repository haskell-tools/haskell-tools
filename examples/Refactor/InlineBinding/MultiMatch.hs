module Refactor.InlineBinding.MultiMatch where

b = a
a x "a" = x
a "a" y = y
a x y = x ++ y