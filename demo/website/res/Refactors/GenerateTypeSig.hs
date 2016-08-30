module Refactors.GenerateTypeSig where

import Data.List
import Data.Ord (comparing)
 
data HTree a = Leaf a | Branch (HTree a) (HTree a)
                deriving Show
 
-- TODO: use [Ctrl, Alt and/or Shift + S] to generate the type signature of the function
-- Bonus: do the same to the two local definitions (hint: if struck try to generate the signature of htree first)

-- | Huffman encoding
huffman freq = sortBy (comparing fst) $ serialize $ htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]
  where serialize (Branch l r) =
                [(x, '0':code) | (x, code) <- serialize l] ++
                [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]
        htree [(_, t)] = t
        htree ((w1,t1):(w2,t2):wts) 
          = htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts

-- source: https://wiki.haskell.org/99_questions/
