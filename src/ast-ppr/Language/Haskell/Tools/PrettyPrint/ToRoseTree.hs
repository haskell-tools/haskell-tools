{-# LANGUAGE LambdaCase
           , MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , ScopedTypeVariables 
           #-}
-- | Generically transforms an ADT into a 'SourceTree'
module Language.Haskell.Tools.PrettyPrint.ToRoseTree where

import GHC.Generics
import Control.Applicative
import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.StructuralTraversal
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.PrettyPrint.AnnInfo
                         
-- | An intermediate data struture. Used to construct a 'SourceRose' tree.
data RoseCollect a = RoseCollect [RoseCollect a]
                   | RoseInfo a
     
instance Show a => Show (RoseCollect a) where
  show = show' 0
    where show' i (RoseCollect children) 
            = "\n" ++ replicate (2*i) '#' ++ concatMap (show' (i+1)) children
          show' i (RoseInfo inf) 
            = "\n" ++ replicate (2*i) '#' ++ show inf
     
-- | Transforms the intermediate representation 'RoseCollect' to a 'RoseTree'     
collectRose :: forall a . (AnnInfo a) 
            => RoseCollect a -> RoseTree a
collectRose = collectRose' (error "No ancestor node has source info.")
  where collectRose' ancestor (RoseCollect coll) 
          = case partitionRoses coll of 
              ([inf], branch) -> RoseTree inf True 
                                          (map (collectRose' inf) branch)
              ([], branch)    -> let children = map (collectRose' ancestor) branch
                                  in RoseTree (generateInfo ancestor (map roseInfo children)) 
                                              False children
                                            
              (infos, branch) -> RoseTree (generateInfo ancestor infos) False 
                                          (map (collectRose' ancestor) branch)
           where -- | Partition collected nodes into info nodes and subnodes
                 partitionRoses :: [RoseCollect a] -> ([a], [RoseCollect a])
                 partitionRoses = partitionEithers 
                                    . (map $ \case c@(RoseCollect _) -> Right c
                                                   RoseInfo i -> Left i)
           
-- | Convert AST tree into a more simpler rose tree, where the nodes are annotated
-- with the node info of the original nodes.
toRose :: (AnnInfo inf, StructuralTraversable n) => n inf -> RoseTree inf
toRose tree = collectRose . toSourceRose $ tree

-- | Creates an intermediate version of the rose tree
toSourceRose :: forall n inf . (AnnInfo inf, StructuralTraversable n) => n inf -> RoseCollect inf
toSourceRose n = evalState (toSrcRoseSt n) [[]]
  where toSrcRoseSt :: n inf -> State [[RoseCollect inf]] (RoseCollect inf)
        toSrcRoseSt n = traverseUp desc asc f n *> gets (RoseCollect . reverse . head)
  
        desc  = modify ([]:)
        asc   = modify (\(x:y:xs) -> ((RoseCollect (reverse x) : y) : xs))
        f inf = modify (\(x:xs) -> (RoseInfo inf : x) : xs)
