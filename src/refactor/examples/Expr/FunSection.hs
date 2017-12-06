module Expr.FunSection where

data Rule = Rule {
    rulePath :: String -> Bool }

f r = filter (`rulePath` r)
