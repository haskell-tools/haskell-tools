{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

module NormalInstance where

import NormalClassDecl

instance (:!:) [a] [b] where  {-* MultiParamTypeClasses *-}
  g _ _ = ()
