{-# LANGUAGE FlexibleInstances
             #-}

module SynonymSameTyVars where

import Definitions


instance C1 (TS2 a a) where      {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (TS4 a b d d) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

instance C1 (HomoTuple a) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True

-- False positive
instance C1 (Phantom a a) where  {-* FlexibleInstances, TypeSynonymInstances *-}
  f1 _ = True
