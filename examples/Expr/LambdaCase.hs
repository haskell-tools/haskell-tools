{-# LANGUAGE LambdaCase, EmptyCase #-}
module Expr.LambdaCase where

a = \case 1 -> 0
          _ -> 1

b = \case { 1 -> 0; _ -> 1 }

c = \case {}

d = \case