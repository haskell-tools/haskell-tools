{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEqualitySynonyms where

import Definitions

type TripleEq a b c = (EqRel a b, EqRel b c) {-* TypeFamilies, TypeFamilies, ConstraintKinds *-}
