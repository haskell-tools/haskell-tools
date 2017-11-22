{-# LANGUAGE FlexibleInstances #-}

module TopLevelWiredInType where

import Definitions


instance C1 Int where  {-* FlexibleInstances *-}
  f1 _ = True
