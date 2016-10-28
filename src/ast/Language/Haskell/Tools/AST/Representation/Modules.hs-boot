{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Representation.Modules where

type role UModule nominal nominal
data UModule dom stage

type role UImportDecl nominal nominal
data UImportDecl dom stage
