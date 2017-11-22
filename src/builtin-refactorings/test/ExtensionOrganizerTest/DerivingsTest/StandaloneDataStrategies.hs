{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             DeriveGeneric,
             DeriveLift,
             DeriveAnyClass,
             StandaloneDeriving,
             DerivingStrategies
             #-}

module StandaloneDataStrategies where

import SynonymDefinitions

deriving stock instance Show    (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving stock instance Read    (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving stock instance Eq      (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving stock instance Enum    (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving stock instance Ord     (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving stock instance Bounded (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving stock instance Ix      (D0' a)  {-* DerivingStrategies, StandaloneDeriving *-}

deriving stock instance Data a     => Data     (D0' a) {-* DerivingStrategies, StandaloneDeriving, DeriveDataTypeable *-}
deriving       instance Typeable a => Typeable (D0' a) {-*                     StandaloneDeriving, DeriveDataTypeable *-}
deriving stock instance Generic a  => Generic  (D0' a) {-* DerivingStrategies, StandaloneDeriving, DeriveGeneric *-}
deriving       instance Lift a     => Lift     (D0' a) {-*                     StandaloneDeriving, DeriveLift *-}
deriving stock instance Functor     D0'                {-* DerivingStrategies, StandaloneDeriving, DeriveFunctor *-}
deriving       instance Foldable    D0'                {-*                     StandaloneDeriving, DeriveFoldable *-}
deriving stock instance Traversable D0'                {-* DerivingStrategies,StandaloneDeriving, DeriveTraversable *-}

deriving anyclass instance C1 (D0' a) {-* DerivingStrategies, StandaloneDeriving, DeriveAnyClass *-}
