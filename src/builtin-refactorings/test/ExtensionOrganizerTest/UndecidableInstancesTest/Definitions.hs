{-# LANGUAGE TypeFamilies #-}

module Definitions where

class C a where
  type T a :: *

class C2 a
class C3 a
class C4 a
class C5 a

type family T1 a

type family CT a where
  CT a = Eq a

data D a b = D a b
