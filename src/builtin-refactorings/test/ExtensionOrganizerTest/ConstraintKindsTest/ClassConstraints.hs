{-# LANGUAGE ConstraintKinds #-}

module ClassConstraints where

type Bar1 a = Eq a            {-* ConstraintKinds *-}
type Bar2 a = (Eq a, Show a)  {-* ConstraintKinds *-}
