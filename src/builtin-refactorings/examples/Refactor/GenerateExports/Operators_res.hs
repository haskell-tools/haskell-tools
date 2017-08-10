{-# LANGUAGE TypeOperators #-}
module Refactor.GenerateExports.Operators ((|=>|), (:+:)(..)) where

(|=>|) :: Int -> Int -> Int
a |=>| b = a + b

data a :+: b = a :+: b