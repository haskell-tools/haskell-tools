module Decl.InlinePragma where

comp :: (b -> c) -> (a -> b) -> a -> c
{-# INLINE CONLIKE [~1] comp #-}
comp f g = \x -> f (g x)