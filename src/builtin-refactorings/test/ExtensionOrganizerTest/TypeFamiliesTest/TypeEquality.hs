{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEquality where

{-@ TypeFamilies, ConstraintKinds @-}

type DoubleEq a b c = (a ~ b, b ~ c) {-* TypeFamilies + GADTs, TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}
