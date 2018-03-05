{-# LANGUAGE TypeOperators,
             FlexibleInstances
             #-}

module SynonymNestedTypes where

import Definitions

{-# ANN module "HLint: ignore Redundant bracket" #-}


instance C1 (TS1 a) where  {-* TypeSynonymInstances *-}
  f1 _ = True

instance C1 (TS1 (TS1 a)) where    {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (TS2 a (TS1 b)) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (TS2 (TS1 a) b) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (PlusSyn (TS1 a) b) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (DoubleList a) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (Phantom (T1 a) a) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

-- False positive
instance C1 (Phantom a (T1 a)) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True
