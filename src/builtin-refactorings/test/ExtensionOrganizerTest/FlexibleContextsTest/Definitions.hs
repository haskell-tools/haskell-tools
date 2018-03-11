{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies, ConstraintKinds #-}

module Definitions where

import GHC.Exts

data T a = T a

class C a where
  foo :: a -> ()

class a :?: b where
  q :: a -> b -> ()

class (a :!: b) c where
  e :: a -> b -> c -> ()

type family TF a :: Constraint
