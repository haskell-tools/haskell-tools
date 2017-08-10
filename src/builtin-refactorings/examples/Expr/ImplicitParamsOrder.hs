{-# LANGUAGE ImplicitParams #-}

module Expr.ImplicitParamsOrder where

x :: IO ()
x = do let ?a = ()
           ?b = ()
       return ()
