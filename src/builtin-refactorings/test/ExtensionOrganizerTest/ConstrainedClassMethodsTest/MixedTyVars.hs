{-# LANGUAGE FlexibleContexts, ConstrainedClassMethods #-}

module MixedTyVars where

import Definitions

class C a where
  op1 :: D (a, b) => a -> b  {-* FlexibleContexts *-}
  op2 :: D b      => b -> a
