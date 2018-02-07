{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, StandaloneDeriving, FlexibleInstances #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  , module Language.Haskell.TH.LanguageExtensions
  ) where

import GHC.Generics
import Control.DeepSeq

import SrcLoc (SrcSpan)

import Data.List
import Data.Monoid
import Data.Function (on)
import qualified Data.Map.Lazy   as LMap
import qualified Data.Map.Strict as SMap (Map)

import qualified SAT.MiniSat as SAT

import Language.Haskell.TH.LanguageExtensions (Extension(..))
import Language.Haskell.Tools.Refactor.Utils.Extensions (expandExtension)

{-# ANN module "HLint: ignore Redundant lambda" #-}


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

-- instance Monoid (a -> a -> Ordering) where
--   mempty      = \x y -> EQ
--   mappend f g = \x y -> f x y <> g x y

toFormula :: LogicalRelation a -> SAT.Formula a
toFormula (LVar x)   = SAT.Var x
toFormula (Not  x)   = SAT.Not . toFormula $ x
toFormula (x :||: y) = (SAT.:||:) (toFormula x) (toFormula y)
toFormula (x :&&: y) = (SAT.:&&:) (toFormula x) (toFormula y)

allToFormula :: [LogicalRelation a] -> SAT.Formula a
allToFormula = SAT.All . map toFormula

determineExtensions :: SMap.Map (LogicalRelation Extension) v -> [Extension]
{- NOTE:
 We calculate all the solutions that require the least number of extension enabled, (comparing on length)
 then for each extension we remove all the extensions implied by it, (mergeImplied)
 finally we select the lexicographically first element. (comparing lists after sorting them)
-}
determineExtensions x = minimal . map toExts $ solution
  where solution = SAT.solve_all . allToFormula . LMap.keys $ x
        toExts   = mergeImplied . map fst . filter snd . LMap.toList
        minimal  = minimumBy ((compare `on` length) <> (compare `on` sort))

        rmImplied :: Extension -> [Extension] -> [Extension]
        rmImplied e = flip (\\) implied
          where implied = delete e $ expandExtension e

        mergeImplied :: [Extension] -> [Extension]
        mergeImplied exts = foldl (flip rmImplied) exts exts
