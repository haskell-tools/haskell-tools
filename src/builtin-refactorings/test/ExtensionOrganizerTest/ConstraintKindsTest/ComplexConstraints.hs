{-# LANGUAGE RankNTypes, KindSignatures, ConstraintKinds #-}

module ComplexConstraints where

import GHC.Exts

type Foo1 (f :: * -> Constraint) b = (Eq b, f b) => b -> b         {-* ConstraintKinds, KindSignatures, KindSignatures, KindSignatures *-}
type Foo2 (f :: * -> Constraint)   = forall b . (Eq b, f b) => b   {-* ConstraintKinds, KindSignatures, KindSignatures, KindSignatures, ExplicitForAll *-}
