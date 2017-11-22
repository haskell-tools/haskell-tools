{-# LANGUAGE FlexibleInstances
             #-}

module NestedUnitTyCon where

import Definitions


instance C1 (T3 a b T0) where  {-* FlexibleInstances *-}
  f1 _ = True

-- FlexibleInstances
instance C1 (T3 a T0 c) where  {-* FlexibleInstances *-}
  f1 _ = True

  -- FlexibleInstances
instance C1 (T3 T0 b c) where  {-* FlexibleInstances *-}
  f1 _ = True
