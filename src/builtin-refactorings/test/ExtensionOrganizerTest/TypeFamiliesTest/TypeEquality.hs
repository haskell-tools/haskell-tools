{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEquality where

type TripleEq a b c = (a ~ b, b ~ c) {-* TypeFamilies, TypeFamilies, TypeFamilies, ConstraintKinds *-}
