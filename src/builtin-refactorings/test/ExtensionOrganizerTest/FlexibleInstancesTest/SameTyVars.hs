{-# LANGUAGE FlexibleInstances
             #-}

module SameTyVars where

import Definitions


instance C1 (T2 a a) where      {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (T4 a b d d) where  {-* FlexibleInstances *-}
  f1 _ = True
