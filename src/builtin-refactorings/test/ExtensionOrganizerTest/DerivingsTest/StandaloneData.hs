{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             DeriveGeneric,
             DeriveLift,
             DeriveAnyClass,
             StandaloneDeriving
             #-}

module StandaloneData where

import SynonymDefinitions

deriving instance Show    (D0' a)  {-* StandaloneDeriving *-}
deriving instance Read    (D0' a)  {-* StandaloneDeriving *-}
deriving instance Eq      (D0' a)  {-* StandaloneDeriving *-}
deriving instance Enum    (D0' a)  {-* StandaloneDeriving *-}
deriving instance Ord     (D0' a)  {-* StandaloneDeriving *-}
deriving instance Bounded (D0' a)  {-* StandaloneDeriving *-}
deriving instance Ix      (D0' a)  {-* StandaloneDeriving *-}

deriving instance Data a     => Data     (D0' a) {-* StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Typeable a => Typeable (D0' a) {-* StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Generic a  => Generic  (D0' a) {-* StandaloneDeriving, DeriveGeneric *-}
deriving instance Lift a     => Lift     (D0' a) {-* StandaloneDeriving, DeriveLift *-}
deriving instance Functor     D0'  {-* StandaloneDeriving, DeriveFunctor *-}
deriving instance Foldable    D0'  {-* StandaloneDeriving, DeriveFoldable *-}
deriving instance Traversable D0'  {-* StandaloneDeriving, DeriveTraversable *-}

deriving instance C1 (D0' a) {-* StandaloneDeriving, DeriveAnyClass *-}
