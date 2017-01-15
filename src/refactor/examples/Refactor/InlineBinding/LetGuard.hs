module Refactor.InlineBinding.LetGuard where

a | let x = 3
  , x == 3
  = ()