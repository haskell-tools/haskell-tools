{-# LANGUAGE MultiParamTypeClasses,
             TypeOperators,
             FlexibleInstances
             #-}

module TopLevelTyVar where

import Definitions

{-# ANN module "HLint: ignore Redundant bracket" #-}

-- There are two matches, because there are two top-level tyvars

instance C2 a a where  {-* FlexibleInstances, FlexibleInstances, MultiParamTypeClasses *-}
  f2 _ _ = True

-- extra check for the brackets around "d"
instance C2 ((a :++: b) c) (d) where  {-* FlexibleInstances, MultiParamTypeClasses, TypeOperators *-}
  f2 _ _ = True
