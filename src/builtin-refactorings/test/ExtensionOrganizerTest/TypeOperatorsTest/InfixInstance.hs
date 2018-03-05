{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

module InfixInstance where

import InfixClassDecl

instance [a] :?: [b] where  {-* TypeOperators, MultiParamTypeClasses *-}
  f _ _ = ()
