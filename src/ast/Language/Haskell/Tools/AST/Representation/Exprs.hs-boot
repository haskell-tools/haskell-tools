{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Representation.Exprs where

type role UExpr nominal nominal
data UExpr dom stage

type role UCmd nominal nominal
data UCmd dom stage

type role UFieldWildcard phantom phantom
data UFieldWildcard dom stage