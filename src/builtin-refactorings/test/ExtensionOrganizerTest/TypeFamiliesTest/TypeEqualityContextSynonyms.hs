{-# LANGUAGE TypeFamilies #-}

module TypeEqualityContextSynonyms where

import Definitions

f :: EqRel a b => a -> b  {-* TypeFamilies, TypeFamilies *-}
f = id

g :: TrfAB a b  {-* TypeFamilies, TypeFamilies *-}
g = id
