module Refactor.InlineBinding.MultiApplied where

b u v = a u v
a x "a" = x
a "a" y = y
a x y = x ++ y