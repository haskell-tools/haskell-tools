module Refactor.RenameDefinition.MergeFields where

data A = B { x :: Double } | C { y :: Double }

data A2 = A2 { x2 :: Double, y2 :: Double }

data A3 = B3 { x3 :: Double } | C3 { y3 :: Int }

f a = case a of B {} -> x a
                C {} -> y a
