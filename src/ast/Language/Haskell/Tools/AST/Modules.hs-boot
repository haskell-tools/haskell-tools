{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Modules where

type role Module nominal nominal
data Module dom stage

type role ImportDecl nominal nominal
data ImportDecl dom stage
