{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, StandaloneDeriving #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  , module Language.Haskell.TH.LanguageExtensions
  ) where

import GHC.Generics
import Control.DeepSeq

import SrcLoc (SrcSpan)

import Data.List
import Data.Function (on)
import qualified Data.Map.Lazy   as LMap
import qualified Data.Map.Strict as SMap (Map)

import qualified SAT.MiniSat as SAT

import Language.Haskell.TH.LanguageExtensions (Extension(..))


deriving instance Ord  Extension
deriving instance Read Extension

infix 6 :||:
infix 7 :&&:

data LogicalRelation a = LVar a
                       | Not (LogicalRelation a)
                       | LogicalRelation a :&&: LogicalRelation a
                       | LogicalRelation a :||: LogicalRelation a
  deriving (Eq, Show, Functor, Ord, Generic, NFData)

lNot :: a -> LogicalRelation a
lNot = Not . LVar

lAnd :: a -> a -> LogicalRelation a
lAnd x y = LVar x :&&: LVar y

lOr :: a -> a -> LogicalRelation a
lOr x y = LVar x :||: LVar y

type ExtMap = SMap.Map (LogicalRelation Extension) [SrcSpan]

instance NFData Extension where
  rnf x = seq x ()

cmpOnResult :: LMap.Map k Bool -> LMap.Map k Bool -> Ordering
cmpOnResult = compare `on` cardinality

cardinality :: LMap.Map k Bool -> Int
cardinality = length . filter (== True) . LMap.elems

toFormula :: LogicalRelation a -> SAT.Formula a
toFormula (LVar x)   = SAT.Var x
toFormula (Not  x)   = SAT.Not . toFormula $ x
toFormula (x :||: y) = (SAT.:||:) (toFormula x) (toFormula y)
toFormula (x :&&: y) = (SAT.:&&:) (toFormula x) (toFormula y)

allToFormula :: [LogicalRelation a] -> SAT.Formula a
allToFormula = SAT.All . map toFormula

determineExtensions :: SMap.Map (LogicalRelation Extension) v -> [Extension]
{- NOTE:
 The sorting is needed to get a deterministic result.
 This way we get all the solutions that require the least number of extension enabled,
 then we select the lexicographically first element.
-}
determineExtensions x = minimum . map toExts . head . minimals $ solution
  where solution = SAT.solve_all . allToFormula . LMap.keys $ x
        toExts = sort . map fst . filter snd . LMap.toList
        minimals = groupBy ((==) `on` cardinality) . sortBy (compare `on` cardinality)
