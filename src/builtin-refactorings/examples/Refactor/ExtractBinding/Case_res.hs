module Refactor.ExtractBinding.Case where

f a = case a of (x,y) -> g x y
  where g x y = x + y