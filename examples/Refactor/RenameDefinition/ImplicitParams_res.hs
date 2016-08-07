{-# LANGUAGE ImplicitParams #-}

module Refactor.RenameDefinition.ImplicitParams where

import Data.List

sort' :: (?compare :: a -> a -> Ordering) => [a] -> [a]
sort' = sortBy ?compare
