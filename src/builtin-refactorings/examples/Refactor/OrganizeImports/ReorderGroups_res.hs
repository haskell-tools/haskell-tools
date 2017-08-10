module Refactor.OrganizeImports.ReorderGroups where

import Control.Monad ((>>=))
import Data.List (intersperse)

import Data.Maybe (catMaybes)
import Data.Tuple (swap)

a = intersperse '-' "abc"
b = Just () >>= \_ -> Nothing
c = catMaybes [Just ()]
d = swap ("a","b")
