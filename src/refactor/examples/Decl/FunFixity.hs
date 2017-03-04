module Decl.FunFixity where

infixl `snoc`
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]
