{-# LANGUAGE PatternSynonyms #-}
module Refactor.RenameDefinition.PatternSynonym where

data Type = App String [Type]

pattern ArrowAppl t1 t2 = App "->" [t1, t2]
