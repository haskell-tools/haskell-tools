{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies, ConstraintKinds, FlexibleContexts, RankNTypes #-}

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

instance C [a] where
  foo = const ()

type Ctxt a = C [a]
type FunWCtxt a = Ctxt a => a -> ()

fCtxt :: FunWCtxt a
fCtxt = const ()

fNestedCtxt :: a -> FunWCtxt a -> ()
fNestedCtxt x f = f x


type SimpleEq a = Eq a
