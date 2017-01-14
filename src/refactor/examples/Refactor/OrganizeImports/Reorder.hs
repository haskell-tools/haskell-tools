module Refactor.OrganizeImports.Reorder where

import Data.List (intersperse)
import Control.Monad ((>>=))

x = intersperse '-' "abc"
y = Just () >>= \_ -> Nothing
