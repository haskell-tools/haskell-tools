module Refactor.ExtractBinding.IndentationMultiLine where

f a = case extracted of 
    Nothing 
      -> 0
    Just x 
      -> x
  where extracted = Just a