{-# LANGUAGE DeriveDataTypeable,
             DeriveTraversable,
             DeriveGeneric,
             DeriveLift,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving,
             TypeSynonymInstances
             #-}

module StandaloneNewtypeSynonyms where

import SynonymDefinitions
import StandaloneDataSynonyms


deriving instance Show    (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Read    (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}

deriving instance Eq      (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Ord     (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Bounded (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Ix      (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}

deriving instance Data a     => Data     (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Typeable a => Typeable (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Generic a  => Generic  (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveGeneric *-}
deriving instance Lift a     => Lift     (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveLift *-}

deriving instance Functor     T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Foldable    T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Traversable T0  {-* TypeSynonymInstances, StandaloneDeriving, DeriveTraversable *-}
