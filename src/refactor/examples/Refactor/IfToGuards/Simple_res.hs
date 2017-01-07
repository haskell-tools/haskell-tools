module Refactor.IfToGuards.Simple where

max a b | a > b = a
        | otherwise = b