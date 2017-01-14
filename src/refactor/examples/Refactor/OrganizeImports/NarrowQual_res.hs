module Refactor.OrganizeImports.NarrowQual where

import Data.List (map, intersperse)
import Data.List as L (map, intersperse)

test = intersperse 0 $ L.map (+1) [1..10]
