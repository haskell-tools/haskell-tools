{-# LANGUAGE TypeFamilies, RankNTypes #-}

module NestedTypeEqualityContextSynonyms where

import Definitions

f :: HiddenEqRel a b => a -> b  {-* TypeFamilies, TypeFamilies *-}
f = id

g :: ComplexEqRelType a  {-* TypeFamilies, TypeFamilies *-}
g x y h = h x y
