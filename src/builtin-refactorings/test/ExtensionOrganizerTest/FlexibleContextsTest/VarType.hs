{-# LANGUAGE FlexibleContexts, ExplicitForAll, KindSignatures, TypeOperators #-}

module VarType where

import Definitions

f :: forall (f :: * -> *) a . C (f a) => f a -> ()  {-* ExplicitForAll, KindSignatures, KindSignatures, KindSignatures *-}
f = foo

g :: forall (f :: * -> *) a . (C (f a), (f a) :?: a) => f a -> ()  {-* ExplicitForAll, KindSignatures, KindSignatures, KindSignatures, TypeOperators *-}
g = foo
