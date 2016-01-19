{-# LANGUAGE PartialTypeSignatures #-}
module Type.Wildcard where

not' :: Bool -> _
not' x = not x
-- Inferred: Bool -> Bool
