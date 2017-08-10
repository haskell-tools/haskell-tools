module Refactor.InlineBinding.LetStmt where

a = do let x = () 
       putStrLn (show x)