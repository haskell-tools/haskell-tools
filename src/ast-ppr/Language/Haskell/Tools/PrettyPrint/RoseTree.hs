{-# LANGUAGE NamedFieldPuns
           , FlexibleContexts 
           #-}
-- | A simpler representation of the original AST. Enables easy relative indexing of the nodes.
module Language.Haskell.Tools.PrettyPrint.RoseTree where

import Control.Monad.State
import Data.StructuralTraversal

-- | A rose tree containing additional node information         
data RoseTree a = RoseTree { roseInfo :: a 
                           , original :: Bool
                           , roseChildren :: [RoseTree a]
                           }

instance Show a => Show (RoseTree a) where
  show sr = show' 0 sr
    where show' i (RoseTree {roseInfo,original,roseChildren})
             = "\n" ++ replicate (2*i) (if original then '#' else '-')
                    ++ show roseInfo 
                    ++ concatMap (show' (i+1)) roseChildren
                    
toRoseTree :: (StructuralTraversable n) => n inf -> RoseTree inf
toRoseTree = head . head . flip execState [[]] . toSrcRoseSt
  where toSrcRoseSt = traverseUp desc (return ()) f
  
        desc  = modify ([]:)
        f inf = modify (\(y:x:xs) -> (RoseTree inf True (reverse y) : x) : xs)
