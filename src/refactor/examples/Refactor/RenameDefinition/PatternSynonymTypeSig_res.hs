{-# LANGUAGE PatternSynonyms #-}
module Refactor.RenameDefinition.PatternSynonymTypeSig where

data Type = App String [Type]

pattern ArrowAppl :: Type -> Type -> Type
pattern ArrowAppl t1 t2 = App "->" [t1, t2]
