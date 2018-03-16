{-# LANGUAGE ExplicitForAll, FlexibleContexts, TypeOperators #-}

module ForAll where

import Definitions

f :: forall a . C [a] => [a] -> ()  {-* FlexibleContexts, FlexibleContexts, ExplicitForAll *-}
f = foo                             {-* FlexibleContexts *-}

g :: forall a . (C [a], [a] :?: [a]) => [a] -> ()  {-* FlexibleContexts, FlexibleContexts, FlexibleContexts, ExplicitForAll, TypeOperators *-}
g = foo                                            {-* FlexibleContexts *-}
