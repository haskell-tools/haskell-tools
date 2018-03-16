{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module TypeApp where

import Definitions

f :: C (T a) => T a -> ()  {-* FlexibleContexts, FlexibleContexts *-}
f = foo                    {-* FlexibleContexts *-}

g :: (C (T a), (T a) :?: (T a)) => T a -> ()  {-* FlexibleContexts, FlexibleContexts, FlexibleContexts, TypeOperators *-}
g = foo                                       {-* FlexibleContexts *-}
