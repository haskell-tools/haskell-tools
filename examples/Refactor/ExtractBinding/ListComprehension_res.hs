module Refactor.ExtractBinding.ListComprehension where

filterPrime (p:xs) = 
    p : filterPrime [ x | x <- xs
                        , notDivisible x ]
  where notDivisible x = x `mod` p /= 0