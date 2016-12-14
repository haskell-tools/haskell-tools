module Refactor.ExtractBinding.Indentation where

f a = case extracted of 
    Nothing -> 0
    Just x -> x
  where extracted = Just a