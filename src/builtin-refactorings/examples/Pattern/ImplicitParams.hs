{-# LANGUAGE ImplicitParams #-}
module Pattern.ImplicitParams where

import Data.List (sortBy)

sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp

main = let ?cmp = compare in putStrLn (show (sort [3,1,2]))