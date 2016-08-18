{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Exprs where

type role Expr nominal nominal
data Expr dom stage

type role Cmd nominal nominal
data Cmd dom stage

type role FieldWildcard phantom phantom
data FieldWildcard dom stage