{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Binds where

type role LocalBind nominal nominal
data LocalBind dom stage

type role LocalBinds nominal nominal
data LocalBinds dom stage

type role RhsGuard nominal nominal
data RhsGuard dom stage