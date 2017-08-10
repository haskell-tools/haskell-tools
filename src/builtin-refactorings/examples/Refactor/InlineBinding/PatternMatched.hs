module Refactor.InlineBinding.PatternMatched where

b u v = a (Just u) v
a (Just x) (Just y) = x ++ y