{-# LANGUAGE UnboxedSums #-}
module Expr.UnboxedSum where

f = ()
  where
    expr :: (# Int | Bool #)
    expr = (# | True #)

    expr2 :: (# Int | Bool #)
    expr2 = (# 3 | #)

    expr3 :: (# Int | String | Bool #)
    expr3 = (# | "aaa" | #)
