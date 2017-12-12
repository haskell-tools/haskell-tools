{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Decl.ViewPatternSynonym where

data Uncert a = Un a a

pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)
