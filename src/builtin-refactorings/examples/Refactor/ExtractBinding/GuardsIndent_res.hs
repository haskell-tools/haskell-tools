module Refactor.ExtractBinding.GuardsIndent where

fn xs
  | length xs == 2 = "2"
  | length xs == 3 = test
  | otherwise = "other"
  where test = "3"