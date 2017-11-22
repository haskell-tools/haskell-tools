{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses
             #-}

module Combined where

import Definitions

-- NOTE: runs really slowly

-- same TyVars and TopLevelTyVar

instance C2 a (T2 c c) where  {-* FlexibleInstances, FlexibleInstances, MultiParamTypeClasses *-}
  f2 _ _ = True
