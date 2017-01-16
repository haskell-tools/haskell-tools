module Refactor.ExtractBinding.AssocOpRightAssoc where

f = g . id
  where g = id . id