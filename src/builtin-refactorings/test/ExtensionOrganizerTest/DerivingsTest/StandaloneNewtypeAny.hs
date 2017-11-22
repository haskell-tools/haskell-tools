{-# LANGUAGE StandaloneDeriving,
             DeriveAnyClass
             #-}

module StandaloneNewtypeAny where

import SynonymDefinitions
import StandaloneDataSynonyms

deriving instance C1 (T0' a) {-* StandaloneDeriving, DeriveAnyClass *-}
