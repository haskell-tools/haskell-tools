module Refactor.ExtractBinding.Guards where

fn xs
  | length xs == 2 = "2"
  | length xs == 3 = "3"
  | otherwise = "other"