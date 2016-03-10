module Refactor.OrganizeImports.SameName where

import Data.List (map)

test = map (+1) [1..null]
  where null = 10
