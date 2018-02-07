{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module TypeEquality where

{-@ GADTs, ConstraintKinds @-}

type DoubleEq a b c = (a ~ b, b ~ c) {-* TypeFamilies + GADTs, TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}
