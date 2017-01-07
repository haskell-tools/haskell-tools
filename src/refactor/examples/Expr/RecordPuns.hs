{-# LANGUAGE NamedFieldPuns #-}
module Expr.RecordPuns where

data Point = Point { x :: Int, y :: Int }

f (Point {y}) = y