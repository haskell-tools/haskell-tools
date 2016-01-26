{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Binds where

type role LocalBind nominal
data LocalBind a

type role LocalBinds nominal
data LocalBinds a

type role RhsGuard nominal
data RhsGuard a