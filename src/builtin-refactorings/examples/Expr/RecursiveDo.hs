{-# LANGUAGE RecursiveDo #-}
module Expr.RecursiveDo where

justOnes = mdo xs <- Just (1:xs)
               return (map negate xs)

justOnes' = do rec xs <- Just (1:xs)
                   return (map negate xs)
               return xs
