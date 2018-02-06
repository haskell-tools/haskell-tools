{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module NestedTypeEqualitySynonyms where

import Definitions

{-@ TypeFamilies, ConstraintKinds @-}

type TripleEq a b c = (HiddenEqRel a b, HiddenEqRel b c) {-* TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}
