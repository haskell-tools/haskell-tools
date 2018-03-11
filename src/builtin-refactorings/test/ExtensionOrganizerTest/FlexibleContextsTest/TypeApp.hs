{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module TypeApp where

import Definitions

f :: C (T a) => T a -> ()  {-* FlexibleContexts *-}
f = foo

g :: (C (T a), (T a) :?: (T a)) => T a -> ()  {-* FlexibleContexts, FlexibleContexts, TypeOperators *-}
g = foo
