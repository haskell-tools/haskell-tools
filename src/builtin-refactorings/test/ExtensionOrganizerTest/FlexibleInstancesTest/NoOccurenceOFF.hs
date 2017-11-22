{-# LANGUAGE KindSignatures,
             TypeOperators,
             MultiParamTypeClasses
             #-}

module NoOccurenceOFF where

import Definitions

{-# ANN module "HLint: ignore Redundant bracket" #-}

-- NOTE: The FlexibleInstancesChecker shouldn't even run,
--       since the FlexibleInstances isn't even turned on.
--       Same test-cases as in NoOccurenceON.

instance (C1 (((T4) (a)) b c d)) where
    f1 _ = True

instance C1 (T2 a b) where
    f1 _ = True

instance C1 (T1 a) where
    f1 _ = True

-- (because T0 is a type ctor here)
instance C1 T0 where
    f1 _ = True

instance C1 (a :+: b) where
  f1 _ = True

instance C1 ((:-:) a b) where
  f1 _ = True

instance (:?:) T0 T0 where
  h _ _ = True

instance T0 :!: T0 where
  j _ _ = True

instance T0 :!: (T1 a) where
  j _ _ = True

instance (T2 a b) :!: (T1 a) where
  j _ _ = True

instance (a :+: b) :!: (T1 a) where
  j _ _ = True

instance C1 [(a :: *)] where
  f1 _ = True

instance C2 (T1 a) (T1 a) where
  f2 _ _ = True
