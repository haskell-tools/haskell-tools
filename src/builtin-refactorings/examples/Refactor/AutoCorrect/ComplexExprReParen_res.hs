module Refactor.AutoCorrect.ComplexExprReParen where

x = (f (f (show ())))

f :: String -> String
f = id