{-# LANGUAGE ImplicitParams #-}

module Refactor.RenameDefinition.ImplicitParams where

import Data.List

sort' :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort' = sortBy ?cmp
