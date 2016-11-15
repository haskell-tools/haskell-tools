module Refactor.InlineBinding.WithLocals where

b = a
a = x y
  where x = id
        y = ()