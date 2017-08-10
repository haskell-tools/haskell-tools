module Refactor.OrganizeImports.ReorderGroups where

import Data.List (intersperse)
import Control.Monad ((>>=))

import Data.Tuple (swap)
import Data.Maybe (catMaybes)

a = intersperse '-' "abc"
b = Just () >>= \_ -> Nothing
c = catMaybes [Just ()]
d = swap ("a","b")
