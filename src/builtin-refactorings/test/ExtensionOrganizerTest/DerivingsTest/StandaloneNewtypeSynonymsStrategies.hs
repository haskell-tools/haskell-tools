{-# LANGUAGE DeriveDataTypeable,
             DeriveTraversable,
             DeriveGeneric,
             DeriveLift,
             GeneralizedNewtypeDeriving,
             DeriveAnyClass,
             StandaloneDeriving,
             TypeSynonymInstances,
             DerivingStrategies
             #-}

module StandaloneNewtypeSynonymsStrategies where

import SynonymDefinitions
import StandaloneDataSynonyms


deriving stock   instance Show    (T0 a)  {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving *-}
deriving newtype instance Read    (T0 a)  {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}

deriving         instance Eq      (T0 a)  {-*                     TypeSynonymInstances, StandaloneDeriving *-}
deriving         instance Ord     (T0 a)  {-*                     TypeSynonymInstances, StandaloneDeriving *-}
deriving newtype instance Bounded (T0 a)  {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving newtype instance Ix      (T0 a)  {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving         instance Enum    (T0 a)  {-*                     TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}


deriving stock   instance Data a     => Data     (T0 a) {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}
deriving newtype instance Typeable a => Typeable (T0 a) {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving stock   instance               Traversable T0  {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, DeriveTraversable *-}
deriving         instance Generic a  => Generic  (T0 a) {-*                     TypeSynonymInstances, StandaloneDeriving, DeriveGeneric *-}
deriving stock   instance Lift a     => Lift     (T0 a) {-* DerivingStrategies, TypeSynonymInstances, StandaloneDeriving, DeriveLift *-}

deriving instance Functor     T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Foldable    T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
