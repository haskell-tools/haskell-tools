{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.TH where
type role Splice nominal
data Splice a
type role QuasiQuote nominal
data QuasiQuote a
type role Bracket nominal
data Bracket a