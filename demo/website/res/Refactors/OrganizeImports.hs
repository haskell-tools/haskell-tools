module Refactors.OrganizeImports where

-- TODO: use [Ctrl, UAlt and/or Shift + O] organize the imports in this module
import qualified Data.Map (filter, null)
import Data.List (inits, tails, intercalate)
-- If the import is empty after the refactoring it is probably not needed, but
-- it stays because some typeclass instances might be used

subsequences :: [a] -> [[a]]
subsequences = filter (not . null) . concat . map inits . tails
