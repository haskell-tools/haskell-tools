{-# LANGUAGE RoleAnnotations #-}
-- | Representation of Haskell types
module Language.Haskell.Tools.AST.Representation.Types where

type role UType nominal nominal
data UType dom stage
