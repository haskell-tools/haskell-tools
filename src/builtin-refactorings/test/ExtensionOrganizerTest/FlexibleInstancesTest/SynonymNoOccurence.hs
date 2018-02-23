{-# LANGUAGE KindSignatures,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances
             #-}

module SynonymNoOccurence where

import Definitions

{-# ANN module "HLint: ignore Redundant bracket" #-}

instance (C1 (((TS4) (a)) b c d)) where  {-* TypeSynonymInstances *-}
    f1 _ = True

instance C1 (TS2 a b) where  {-* TypeSynonymInstances *-}
    f1 _ = True

instance C1 (TS1 a) where  {-* TypeSynonymInstances *-}
    f1 _ = True

-- (because TS0 is a type ctor here)
instance C1 TS0 where  {-* TypeSynonymInstances *-}
    f1 _ = True

instance C1 (PlusSyn a b) where  {-* TypeSynonymInstances *-}
  f1 _ = True

instance C1 ((:-:) a b) where
  f1 _ = True                   

instance (:?:) TS0 TS0 where    {-* MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances *-}
  h _ _ = True

instance TS0 :!: TS0 where      {-* MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances, TypeOperators *-}
  j _ _ = True

instance TS0 :!: (TS1 a) where  {-* MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances, TypeOperators *-}
  j _ _ = True

instance (TS2 a b) :!: (TS1 a) where  {-* MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances, TypeOperators *-}
  j _ _ = True

instance (a :+: b) :!: (TS1 a) where  {-* MultiParamTypeClasses, TypeSynonymInstances, TypeOperators, TypeOperators *-}
  j _ _ = True

instance C1 [(a :: *)] where         {-* KindSignatures *-}
  f1 _ = True

instance C2 (TS1 a) (TS1 a) where      {-* MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances *-}
  f2 _ _ = True
