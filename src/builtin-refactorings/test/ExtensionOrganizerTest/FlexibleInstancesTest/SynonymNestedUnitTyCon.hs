{-# LANGUAGE FlexibleInstances
             #-}

module SynonymNestedUnitTyCon where

import Definitions


instance C1 (TS3 a b TS0) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (TS3 a TS0 c) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (TS3 TS0 b c) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (DoubleList T0) where  {-* FlexibleInstances, FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (HomoTuple T0) where  {-* FlexibleInstances, FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (Phantom T0 a) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

-- False positive
instance C1 (Phantom a T0) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True
