module Decl.SpecializePragma where

hammeredLookup :: Ord key => [(key, value)] -> key -> value
{-# SPECIALIZE hammeredLookup :: [(String, value)] -> String -> value, [(Char, value)] -> Char -> value #-}
hammeredLookup = undefined
