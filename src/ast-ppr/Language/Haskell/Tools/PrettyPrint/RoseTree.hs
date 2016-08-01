{-# LANGUAGE NamedFieldPuns
           , FlexibleContexts 
           , DeriveFunctor
           , StandaloneDeriving
           #-}
-- | A simpler representation of the original AST.
module Language.Haskell.Tools.PrettyPrint.RoseTree where

import Control.Monad.State
import Language.Haskell.Tools.AST

-- | A rose tree containing additional node information         
data RoseTree st = RoseTree { roseInfo :: RoseSourceInfo st 
                            , roseChildren :: [RoseTree st]
                            }

-- | Heterogenous representation of source information, for pretty printing
data RoseSourceInfo st
  = RoseSpan (SpanInfo st)
  | RoseList (ListInfo st)
  | RoseOptional (OptionalInfo st)

deriving instance SourceInfo st => Show (RoseSourceInfo st)

instance SourceInfo st => Show (RoseTree st) where
  show = show' 0
    where show' i RoseTree{roseInfo,roseChildren}
             = "\n" ++ replicate (2*i) '#'
                    ++ show roseInfo 
                    ++ concatMap (show' (i+1)) roseChildren
                          
-- | Transforms the heterogeneous AST into a homogeneous representation for pretty printing                   
toRoseTree :: (SourceInfoTraversal n) => n dom st -> RoseTree st
toRoseTree = head . head . tail . flip execState [[],[]] . toSrcRoseSt
  where toSrcRoseSt = sourceInfoTraverseUp (SourceInfoTrf (trf RoseSpan) (trf RoseList) (trf RoseOptional)) desc asc
  
        desc  = modify ([]:)
        asc   = modify tail
        trf wrap inf = do modify (\(y : x : xs) -> [] : (RoseTree (wrap inf) (reverse y) : x) : xs)
                          return inf
