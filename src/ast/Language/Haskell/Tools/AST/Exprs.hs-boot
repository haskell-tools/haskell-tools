{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Exprs where

type role Expr nominal
data Expr a

type role Cmd nominal
data Cmd a