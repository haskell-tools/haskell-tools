{-# LANGUAGE FlexibleInstances #-}

module NestedWiredInType where

import Definitions


instance C1 (T3 a b Int) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (T3 a Int c) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (T3 Int b c) where  {-* FlexibleInstances *-}
  f1 _ = True
