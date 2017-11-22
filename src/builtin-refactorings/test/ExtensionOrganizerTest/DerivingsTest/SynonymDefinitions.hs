module SynonymDefinitions
  ( module SynonymDefinitions
  , module Definitions
  ) where

import Definitions


data D0' a = D0'

type D0 = D0'

newtype T0' a = T0 (D0' a)

type T0 = T0'
