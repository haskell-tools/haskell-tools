module Refactor.InlineBinding.MultiMatch where

b = (\x1 x2 -> case (x1, x2) of (x, "a") -> x
                                ("a", y) -> y
                                (x, y) -> x ++ y)