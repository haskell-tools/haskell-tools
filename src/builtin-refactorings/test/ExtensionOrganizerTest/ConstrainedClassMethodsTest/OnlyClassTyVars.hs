{-# LANGUAGE ConstrainedClassMethods #-}

module OnlyClassTyVars where

import Definitions

class C a where       {-* ConstrainedClassMethods *-}
  op :: D a => a -> b
