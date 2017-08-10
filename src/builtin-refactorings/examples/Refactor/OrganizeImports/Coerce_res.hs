module Refactor.OrganizeImports.Coerce where

import Data.Coerce (coerce)
import Data.List
import Data.Maybe (Maybe(Just, Nothing))
import Prelude ()

newtype Mayb' a = Mayb' (Maybe a)

a :: Mayb' a -> Maybe a
a = coerce
