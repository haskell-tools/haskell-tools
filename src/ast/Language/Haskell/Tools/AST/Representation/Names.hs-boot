{-# LANGUAGE RoleAnnotations #-}

module Language.Haskell.Tools.AST.Representation.Names where

type role UQualifiedName nominal nominal
data UQualifiedName dom stage
