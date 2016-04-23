module Refactor.RenameDefinition.ClassMember where

class C t where
  q :: t -> t

instance C Int where
  q i = i