{-# LANGUAGE StandaloneDeriving,
             DeriveAnyClass,
             TypeSynonymInstances
             #-}

module StandaloneNewtypeSynonymsAny where

import SynonymDefinitions
import StandaloneDataSynonyms

deriving instance C1 (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveAnyClass *-}
