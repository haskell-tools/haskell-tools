{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}
module Refactor.RenameDefinition.IllegalQualRename where

type family IfThenElse (b :: Bool) (th :: x) (el :: x) :: x where
  IfThenElse True  th el = th
  IfThenElse False th el = el