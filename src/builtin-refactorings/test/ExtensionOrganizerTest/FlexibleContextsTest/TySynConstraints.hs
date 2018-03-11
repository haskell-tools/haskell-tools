{-# LANGUAGE FlexibleContexts, ConstraintKinds, TypeOperators, TypeFamilies, RankNTypes #-}

module TySynConstraints where

import GHC.Exts
import Definitions

type CT1 a = Eq [a]             {-* FlexibleContexts, ConstraintKinds *-}
type CT2 a = (Eq [a])           {-* FlexibleContexts, ConstraintKinds *-}
type CT3 a = (Eq a, Ord [a])    {-* FlexibleContexts, ConstraintKinds *-}
type CT4 a = (Eq a, Ord [a])    {-* FlexibleContexts, ConstraintKinds *-}
type CT5 a = a :?: T a          {-* FlexibleContexts, ConstraintKinds, TypeOperators *-}
type CT6 a = (a :!: a) (T a)    {-* FlexibleContexts, ConstraintKinds, TypeOperators *-}
type CT7 a = Eq [a] => a        {-* FlexibleContexts *-}


type CTTF1 a b = [a] ~ b        {-* TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}
type CTTF2 a = (TF [a], TF (T a), TF a ~ Eq [a])  {-* TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}

type CTTyVarHeads (f :: * -> Constraint) a = f a  {-* KindSignatures, KindSignatures, KindSignatures, ConstraintKinds *-}
