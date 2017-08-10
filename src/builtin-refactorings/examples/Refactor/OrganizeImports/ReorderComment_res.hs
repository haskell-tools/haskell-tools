module Refactor.OrganizeImports.ReorderComment where

import Control.Monad ((>>=))
import Data.List (intersperse)
import Data.Maybe (catMaybes)
-- some comment
import Data.Tuple (swap)

a = intersperse '-' "abc"
b = Just () >>= \_ -> Nothing
c = catMaybes [Just ()]
d = swap ("a","b")
