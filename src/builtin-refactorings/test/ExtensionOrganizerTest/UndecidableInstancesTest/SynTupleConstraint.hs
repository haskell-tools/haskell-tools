{-# LANGUAGE ConstraintKinds, FlexibleInstances, UndecidableInstances #-}

module SynTupleConstraint where

import Definitions

type Syn a = (Ord a, Eq a)  {-* ConstraintKinds *-}

instance Syn a => C a       {-* UndecidableInstances, FlexibleInstances *-}
