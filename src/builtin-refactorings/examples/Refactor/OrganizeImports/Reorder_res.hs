module Refactor.OrganizeImports.Reorder where

import Control.Monad ((>>=))
import Data.List (intersperse)

x = intersperse '-' "abc"
y = Just () >>= \_ -> Nothing
