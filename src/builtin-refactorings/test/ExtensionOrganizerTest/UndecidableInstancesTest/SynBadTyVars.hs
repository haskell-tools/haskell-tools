{-# LANGUAGE ConstraintKinds, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

module SynBadTyVars where

import Definitions


type SynC a = C (a,a)  {-* ConstraintKinds, FlexibleContexts, FlexibleContexts *-}

instance SynC (a,a) => C a  {-* UndecidableInstances, FlexibleInstances, FlexibleContexts *-}
