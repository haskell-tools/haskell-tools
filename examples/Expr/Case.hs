{-# LANGUAGE EmptyCase #-}
module Expr.Case where

x = 12

a = case x of 1 -> 0
              _ -> 1

b = case x of { 1 -> 0; _ -> 1 }

--c = case x of {}

--d = case x of