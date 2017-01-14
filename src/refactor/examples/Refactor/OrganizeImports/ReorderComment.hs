module Refactor.OrganizeImports.ReorderComment where

import Data.List (intersperse)
import Control.Monad ((>>=))
-- some comment
import Data.Tuple (swap)
import Data.Maybe (catMaybes)

a = intersperse '-' "abc"
b = Just () >>= \_ -> Nothing
c = catMaybes [Just ()]
d = swap ("a","b")
