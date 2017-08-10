module Refactor.OrganizeImports.Coerce where

import Prelude ()
import Data.Maybe (Maybe(Just, Nothing))
import Data.List
import Data.Coerce (coerce)

newtype Mayb' a = Mayb' (Maybe a)

a :: Mayb' a -> Maybe a
a = coerce
