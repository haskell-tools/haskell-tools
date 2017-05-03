module Refactor.RenameDefinition.MergeFields where

data A = B { y :: Double } | C { y :: Double }

data A2 = A2 { x2 :: Double, y2 :: Double }

data A3 = B3 { x3 :: Double } | C3 { y3 :: Int }

f a = case a of B {} -> y a
                C {} -> y a
