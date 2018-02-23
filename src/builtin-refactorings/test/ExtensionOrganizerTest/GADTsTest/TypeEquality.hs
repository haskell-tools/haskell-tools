{-# LANGUAGE GADTs, ConstraintKinds #-}

module TypeEquality where

{-@ GADTs, ConstraintKinds @-}

type EqRel a b = a ~ b {-* TypeFamilies + GADTs, TypeFamilies + GADTs, ConstraintKinds *-}

data T a where
  T1 :: b -> T a       {-* GADTSyntax, GADTs + ExistentialQuantification *-}
