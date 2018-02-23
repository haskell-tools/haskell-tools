{-# LANGUAGE ConstrainedClassMethods #-}

module NullaryConstraint where

import Definitions

class C a where
  op :: C0 => a -> a
