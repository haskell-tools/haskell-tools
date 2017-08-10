module Refactor.InlineBinding.MultiMatchGuarded where

b u v = a u v
a x y | x == y
      = x
a x y = x ++ y