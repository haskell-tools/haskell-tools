{-# LANGUAGE ParallelListComp #-}
module Expr.ParListComp where

ls = [ x+y | x <- [1..5] | y <- [1..5]]