module Refactor.InlineBinding.WithLocals where

b = (let x = id
         y = () in x y)