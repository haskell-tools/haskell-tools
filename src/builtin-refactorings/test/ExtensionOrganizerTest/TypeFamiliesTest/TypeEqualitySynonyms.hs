{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEqualitySynonyms where

import Definitions

{-@ TypeFamilies, ConstraintKinds @-}

type DoubleEq a b c = (EqRel a b, EqRel b c) {-* TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}
