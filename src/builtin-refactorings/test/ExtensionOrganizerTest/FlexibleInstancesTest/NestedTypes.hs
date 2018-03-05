{-# LANGUAGE TypeOperators,
             FlexibleInstances
             #-}

module NestedTypes where

import Definitions

{-# ANN module "HLint: ignore Redundant bracket" #-}


instance C1 (T1 a) where
  f1 _ = True

instance C1 (T1 (T1 a)) where    {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (T2 a (T1 b)) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (T2 (T1 a) b) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 ((T1 a) :+: b) where  {-* FlexibleInstances, TypeOperators *-}
  f1 _ = True
