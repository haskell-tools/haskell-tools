module Refactor.ExtractBinding.ListComprehension where

filterPrime (p:xs) = 
  p : filterPrime [ x | x <- xs
                      , x `mod` p /= 0 ]