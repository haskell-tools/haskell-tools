module Refactor.ExtractBinding.SiblingDefs where

f = 1
  where
    g = a
      where a = 1
    h = a
      where a = 2
