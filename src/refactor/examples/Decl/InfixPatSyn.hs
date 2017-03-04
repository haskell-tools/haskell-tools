{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Decl.InfixPatSyn where

pattern x :. xs <- (uncons -> Just (x,xs)) where
  x:.xs = cons x xs
infixr 5 :.

cons x xs = x:xs
uncons (x:xs) = Just (x,xs)
uncons [] = Nothing
