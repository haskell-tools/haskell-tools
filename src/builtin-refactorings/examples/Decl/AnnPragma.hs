module Decl.AnnPragma where
{-# ANN module (Just "Hello") #-}
{-# ANN f (Just "Hello") #-}
f :: Int -> Int
f a = a + 1

{-# ANN type A (Just "Hello") #-}
data A = A