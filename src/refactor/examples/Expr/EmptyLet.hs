module Expr.EmptyLet where

a = let in ()

m = do let
       putStrLn "hello"
