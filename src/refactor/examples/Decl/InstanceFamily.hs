{-# LANGUAGE TypeOperators, TypeFamilies #-}
module Decl.InstanceFamily where

import GHC.Generics

class HasTrie a where
  data (:->:) a :: * -> *

instance (HasTrie (f x)) => HasTrie (M1 i t f x) where
  data (M1 i t f x :->: b) = M1Trie (f x :->: b)
