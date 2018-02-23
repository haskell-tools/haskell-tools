{-# LANGUAGE RoleAnnotations #-}

module Language.Haskell.Tools.AST.Representation.TH where

type role USplice nominal nominal
data USplice dom stage

type role UQuasiQuote nominal nominal
data UQuasiQuote dom stage

type role UBracket nominal nominal
data UBracket dom stage