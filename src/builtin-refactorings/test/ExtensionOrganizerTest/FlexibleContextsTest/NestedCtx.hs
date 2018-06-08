{-# LANGUAGE RankNTypes, FlexibleContexts, TypeOperators #-}

module NestedCtx where

import Definitions

f :: (C [a] => [a] -> ()) -> ()  {-* FlexibleContexts, FlexibleContexts *-}
f = undefined                    {-* FlexibleContexts *-}

g :: ((C [a], [a] :?: [a]) => [a] -> ()) -> ()  {-* FlexibleContexts, FlexibleContexts, FlexibleContexts, TypeOperators *-}
g = undefined                                   {-* FlexibleContexts *-}
