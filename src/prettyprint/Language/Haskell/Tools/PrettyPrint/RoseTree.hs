{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns #-}

-- | A simpler representation of the original AST.
module Language.Haskell.Tools.PrettyPrint.RoseTree where

import Control.Exception (Exception(..), throw)
import Control.Monad.State (Monad(..), execState, modify)
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

-- | Transforms the heterogeneous AST into a homogeneous representation for pretty printing
toRoseTree :: (SourceInfoTraversal n) => n dom st -> RoseTree st
toRoseTree = (\case (root:_):_ -> root; _ -> pprProblem "toRoseTree: the result has no root")
                . tail . flip execState [[],[]] . toSrcRoseSt
  where toSrcRoseSt = sourceInfoTraverseUp (SourceInfoTrf (trf RoseSpan) (trf RoseList) (trf RoseOptional)) desc asc

        desc  = modify ([]:)
        asc   = modify tail
        trf wrap inf = do modify (\(y : x : xs) -> [] : (RoseTree (wrap inf) (reverse y) : x) : xs)
                          return inf

data PrettyPrintProblem = PrettyPrintProblem String
  deriving Show

instance Exception PrettyPrintProblem

pprProblem :: String -> a
pprProblem = throw . PrettyPrintProblem
