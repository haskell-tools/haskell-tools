{-# LANGUAGE DeriveFunctor,
             DeriveDataTypeable,
             DeriveTraversable,
             DeriveGeneric,
             DeriveLift,
             GeneralizedNewtypeDeriving,
             DeriveAnyClass,
             StandaloneDeriving,
             DerivingStrategies
             #-}

module StandaloneNewtypeStrategies where

import SynonymDefinitions
import StandaloneData


deriving stock   instance Show    (T0' a)  {-* DerivingStrategies, StandaloneDeriving *-}
deriving newtype instance Read    (T0' a)  {-* DerivingStrategies, StandaloneDeriving, GeneralizedNewtypeDeriving *-}

deriving         instance Eq      (T0' a)  {-*                     StandaloneDeriving *-}
deriving         instance Ord     (T0' a)  {-*                     StandaloneDeriving *-}
deriving newtype instance Bounded (T0' a)  {-* DerivingStrategies, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving newtype instance Ix      (T0' a)  {-* DerivingStrategies, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving         instance Enum    (T0' a)  {-*                     StandaloneDeriving, GeneralizedNewtypeDeriving *-}

deriving stock   instance Data a     => Data     (T0' a) {-* DerivingStrategies, StandaloneDeriving, DeriveDataTypeable *-}
deriving newtype instance Typeable a => Typeable (T0' a) {-* DerivingStrategies, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving stock   instance               Traversable T0'  {-* DerivingStrategies, StandaloneDeriving, DeriveTraversable *-}
deriving         instance Generic a  => Generic  (T0' a) {-*                     StandaloneDeriving, DeriveGeneric *-}
deriving stock   instance Lift a     => Lift     (T0' a) {-* DerivingStrategies, StandaloneDeriving, DeriveLift *-}

deriving stock   instance Functor     T0'    {-* DerivingStrategies, StandaloneDeriving, DeriveFunctor *-}
deriving         instance Foldable    T0'    {-*                     StandaloneDeriving, GeneralizedNewtypeDeriving *-}

deriving newtype instance C1 a => C1 (T0' a) {-* DerivingStrategies, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
