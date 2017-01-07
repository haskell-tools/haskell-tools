{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
module Refactor.RenameDefinition.TypeOperators where

type family x1 :++: l2 where
  '[] :++: l2       = l2
  (e ': r1) :++: l2 = e ': (r1 :++: l2)
  