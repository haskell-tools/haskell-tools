{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module NestedTypeEqualitySynonyms where

import Definitions

type TripleEq a b c = (HiddenEqRel a b, HiddenEqRel b c) {-* TypeFamilies, TypeFamilies *-}
