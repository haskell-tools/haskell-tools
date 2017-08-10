{-# LANGUAGE PatternSynonyms #-}
module Refactor.RenameDefinition.PatternSynonymTypeSig where

data Type = App String [Type]

pattern Arrow :: Type -> Type -> Type
pattern Arrow t1 t2 = App "->" [t1, t2]
