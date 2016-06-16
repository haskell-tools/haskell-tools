module Refactor.ExtractBinding.ListComprehension where

filterPrime (p:xs) = 
  p : filterPrime [ x | x <- xs
                      , notDivisible x p ]
notDivisible x p = x `mod` p /= 0