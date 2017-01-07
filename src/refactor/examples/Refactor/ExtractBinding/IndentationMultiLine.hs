module Refactor.ExtractBinding.IndentationMultiLine where

f a = case Just a of 
  Nothing 
    -> 0
  Just x 
    -> x