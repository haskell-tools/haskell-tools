{-# LANGUAGE NPlusKPatterns #-}
module Pattern.NPlusK where

factorial :: Integer -> Integer
factorial 0 = 1
factorial (n+1) = (*) (n+1) (factorial n)