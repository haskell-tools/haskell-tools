{-# LANGUAGE MultiWayIf #-}
module Expr.MultiwayIf where

b = 12
a = if | b > 3     -> 1
       | b < -5    -> 2
       | otherwise -> 3