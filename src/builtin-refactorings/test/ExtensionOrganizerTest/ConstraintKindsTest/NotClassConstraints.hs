{-# LANGUAGE RankNTypes, KindSignatures, ConstraintKinds #-}

module NotClassConstraints where

import GHC.Exts

type Foo0 (f ::      Constraint)   = f                    {-* ConstraintKinds, KindSignatures *-}
type Foo1 (f :: * -> Constraint) b = f b                  {-* ConstraintKinds, KindSignatures, KindSignatures, KindSignatures *-}
type Foo2 (f :: * -> Constraint) b = f b => b -> b        {-* ConstraintKinds, KindSignatures, KindSignatures, KindSignatures *-}
type Foo3 (f :: * -> Constraint)   = forall b . f b => b  {-* ConstraintKinds, KindSignatures, KindSignatures, KindSignatures *-}
