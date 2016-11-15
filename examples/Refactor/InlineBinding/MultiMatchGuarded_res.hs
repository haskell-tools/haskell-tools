module Refactor.InlineBinding.MultiMatchGuarded where

b u v = (case (u, v) of (x, y) | x == y -> x
                        (x, y) -> x ++ y)