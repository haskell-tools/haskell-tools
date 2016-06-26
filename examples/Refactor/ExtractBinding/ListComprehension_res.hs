module Refactor.ExtractBinding.ListComprehension where

filterPrime (p:xs) = 
  p : filterPrime [ x | x <- xs
                      , notDivisible x p ]
  where notDivisible x p = x `mod` p /= 0