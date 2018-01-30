{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEqualitySynonyms where

import Definitions

type DoubleEq a b c = (EqRel a b, EqRel b c) {-* TypeFamilies, TypeFamilies, ConstraintKinds *-}
