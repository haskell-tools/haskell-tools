{-# LANGUAGE RoleAnnotations #-}
module Decl.TypeRole where

type role Foo representational representational
data Foo a b = Foo Int
