module Refactor.InlineBinding.PatternMatched where

b u v = ((\(Just y) -> u ++ y) v)