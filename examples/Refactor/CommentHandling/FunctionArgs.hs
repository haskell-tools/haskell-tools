module Refactor.CommentHandling.FunctionArgs where

f :: Int -- something
  -> {-| result -} Int
  -> Int  -- ^ other thing
f = undefined
