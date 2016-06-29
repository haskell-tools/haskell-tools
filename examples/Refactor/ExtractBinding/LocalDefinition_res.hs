module Refactor.ExtractBinding.LocalDefinition where

stms = x
  where x = y
          where y = "s"
