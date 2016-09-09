{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Base where

type role QualifiedName nominal nominal
data QualifiedName dom stage