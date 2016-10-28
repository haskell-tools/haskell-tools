{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Representation.Binds where

type role ULocalBind nominal nominal
data ULocalBind dom stage

type role ULocalBinds nominal nominal
data ULocalBinds dom stage

type role URhsGuard nominal nominal
data URhsGuard dom stage