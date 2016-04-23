module Refactor.RenameDefinition.ClassMember where

class C t where
  f :: t -> t

instance C Int where
  f i = i