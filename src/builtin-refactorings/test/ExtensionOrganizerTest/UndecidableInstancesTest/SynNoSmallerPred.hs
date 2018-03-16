{-# LANGUAGE ConstraintKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, ExplicitForAll #-}

module SynNoSmallerPred where

import Definitions

type Syn1 a   = Eq a                       {-* ConstraintKinds *-}
type Syn2 a b = Eq (D [a] b)               {-* ConstraintKinds, FlexibleContexts, FlexibleContexts *-}
type Syn3     = Syn2 Int Int               {-* ConstraintKinds, FlexibleContexts, FlexibleContexts *-}

instance Syn1 a => C a                     {-* UndecidableInstances, FlexibleInstances *-}
instance Syn2 a b => C2 (a,b)              {-* UndecidableInstances, FlexibleContexts *-}
instance forall a b . Syn2 a b => C3 (a,b) {-* UndecidableInstances, FlexibleContexts *-}
instance Syn3 => C4 (Int,Int)              {-* UndecidableInstances, FlexibleInstances, FlexibleContexts *-}
