module Expr.SccPragma where

f x = {-# SCC "drawComponent" #-}
        case x of () -> ()
