{-# LANGUAGE RankNTypes, FlexibleContexts, TypeOperators #-}

module NestedCtx where

import Definitions

f :: (C [a] => [a] -> ()) -> ()  {-* FlexibleContexts *-}
f = undefined

g :: ((C [a], [a] :?: [a]) => [a] -> ()) -> ()  {-* FlexibleContexts, FlexibleContexts, TypeOperators *-}
g = undefined
