{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.TH where

type role Splice nominal nominal
data Splice dom stage

type role QuasiQuote nominal nominal
data QuasiQuote dom stage

type role Bracket nominal nominal
data Bracket dom stage