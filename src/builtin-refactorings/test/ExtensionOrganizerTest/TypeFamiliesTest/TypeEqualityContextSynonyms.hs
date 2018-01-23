{-# LANGUAGE TypeFamilies #-}

module TypeEqualityContextSynonyms where

import Definitions

f :: EqRel a b => a -> b  {-* TypeFamilies *-}
f = id

g :: TrfAB a b  {-* TypeFamilies *-}
g = id
