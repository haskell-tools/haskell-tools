{-# LANGUAGE ExplicitForAll, FlexibleContexts, TypeOperators #-}

module ForAll where

import Definitions

f :: forall a . C [a] => [a] -> ()  {-* FlexibleContexts, ExplicitForAll *-}
f = foo

g :: forall a . (C [a], [a] :?: [a]) => [a] -> ()  {-* FlexibleContexts, FlexibleContexts, ExplicitForAll, TypeOperators *-}
g = foo
