{-# LANGUAGE TypeFamilies, RankNTypes #-}

module NestedTypeEqualityContextSynonyms where

import Definitions

{-@ GADTs @-}

f :: HiddenEqRel a b => a -> b  {-* TypeFamilies + GADTs, TypeFamilies + GADTs *-}
f = id

g :: ComplexEqRelType a  {-* TypeFamilies + GADTs, TypeFamilies + GADTs *-}
g x y h = h x y          {-* TypeFamilies + GADTs, (TypeFamilies + GADTs) *-}
