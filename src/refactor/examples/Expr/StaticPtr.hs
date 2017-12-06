{-# LANGUAGE StaticPointers #-}
module Expr.StaticPtr where

import GHC.StaticPtr

inc :: Int -> Int
inc x = x + 1

ref1, ref3, ref4 :: StaticPtr Int
ref2 :: StaticPtr (Int -> Int)
ref5 :: Int -> StaticPtr Int
ref1 = static 1
ref2 = static inc
ref3 = static (inc 1)
ref4 = static ((\x -> x + 1) (1 :: Int))
ref5 y = static (let x = 1 in x)