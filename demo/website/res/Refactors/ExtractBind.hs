module Refactors.ExtractBind where

-- | Partition elements into disjoint subsets
group :: [Int] -> [a] -> [[[a]]]
group [] = const [[]]
group (n:ns) = concatMap (uncurry $ (. group ns) . map . (:)) . combination n

-- | Select n elements from a list
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
-- TODO: use [Ctrl, UAlt and/or Shift + B] to extract the selected expression as 
-- a local binding. The first list is the possible results if the current element 
-- is selected. The second is the possible results if the current element is not 
-- selected. UName the bindings accordingly.
combination n (x:xs) = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ] ++ [ (ys,x:zs) | (ys,zs) <- combination n xs ]

-- source: https://wiki.haskell.org/99_questions/
