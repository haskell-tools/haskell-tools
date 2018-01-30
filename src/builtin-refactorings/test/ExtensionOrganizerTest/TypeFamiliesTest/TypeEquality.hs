{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEquality where

type DoubleEq a b c = (a ~ b, b ~ c) {-* TypeFamilies, TypeFamilies, TypeFamilies, ConstraintKinds *-}
