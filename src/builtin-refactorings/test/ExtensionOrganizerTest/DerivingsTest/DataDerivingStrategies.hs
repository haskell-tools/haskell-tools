{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             DeriveGeneric,
             DeriveLift,
             DeriveAnyClass,
             DerivingStrategies
             #-}

module DataDerivingStrategies where

import Definitions


data D1 a = D1 a
  deriving Show

data D2 a = D2 a
  deriving stock Eq {-* DerivingStrategies *-}

data D3 a = D3 a
  deriving Data     {-* DeriveDataTypeable *-}

data D4 a = D4 a
  deriving Functor  {-* DeriveFunctor *-}

data D5 a = D5 a
  deriving stock    (Eq, Ord, Typeable, Foldable) {-* DerivingStrategies, DeriveDataTypeable, DeriveFoldable *-}
  deriving anyclass C1                            {-* DerivingStrategies, DeriveAnyClass *-}

data D6 a = D6
  deriving (Eq, Ord, Enum, Ix, Bounded, Show, Read,
            Data, Typeable, Functor, Foldable, Traversable, {-* DeriveDataTypeable, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable *-}
            Generic, Lift)                                  {-* DeriveGeneric, DeriveLift *-}
