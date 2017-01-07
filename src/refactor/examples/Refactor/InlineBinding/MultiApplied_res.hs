module Refactor.InlineBinding.MultiApplied where

b u v = (case (u, v) of (x, "a") -> x
                        ("a", y) -> y
                        (x, y) -> x ++ y)