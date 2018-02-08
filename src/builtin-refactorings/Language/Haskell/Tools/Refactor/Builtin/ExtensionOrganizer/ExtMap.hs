{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, StandaloneDeriving, FlexibleInstances #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  , module Language.Haskell.TH.LanguageExtensions
  , Formula(..)
  ) where

import GHC.Generics
import Control.DeepSeq

import SrcLoc (SrcSpan)

import Data.List
import Data.Monoid hiding (All)
import Data.Function (on)
import qualified Data.Map.Lazy   as LMap
import qualified Data.Map.Strict as SMap (Map)

import SAT.MiniSat

import Language.Haskell.TH.LanguageExtensions (Extension(..))
import Language.Haskell.Tools.Refactor.Utils.Extensions (expandExtension)

{-# ANN module "HLint: ignore Redundant lambda" #-}


deriving instance Ord  Extension
deriving instance Read Extension

instance NFData Extension where
  rnf x = seq x ()

deriving instance Generic a => Generic (Formula a)
deriving instance (Generic a, NFData a) => NFData (Formula a)

type LogicalRelation a = Formula a

type ExtMap = SMap.Map (LogicalRelation Extension) [SrcSpan]

lVar :: a -> LogicalRelation a
lVar = Var

lNot :: a -> LogicalRelation a
lNot = Not . Var

lAnd :: a -> a -> LogicalRelation a
lAnd x y = Var x :&&: Var y

lOr :: a -> a -> LogicalRelation a
lOr x y = Var x :||: Var y

lImplies :: a -> a -> LogicalRelation a
lImplies x y = Var x :->: Var y

lAll :: [LogicalRelation a] -> LogicalRelation a
lAll = All

complexity :: Extension -> Int
complexity = length . expandExtension

determineExtensions :: SMap.Map (LogicalRelation Extension) v -> [Extension]
{- NOTE:
 We calculate all the possible extension set that satisfy the logical relation,
 then for each extension we remove all the extensions implied by it, (mergeImplied)
 finally we select the minimal extension set.

 An extension set is minimal if:
  - it contains the least amount of extensions
  - it contains extension with minimal complexity
  - it is lexicographically the first one (based on the Ord instance of Extension)
-}
determineExtensions x = minimal . map toExts $ solution
  where solution = solve_all . All . LMap.keys $ x
        toExts   = mergeImplied . map fst . filter snd . LMap.toList
        minimal  = minimumBy ((compare `on` length) <> (compare `on` (sum . map complexity)) <> (compare `on` sort))

rmImplied :: Extension -> [Extension] -> [Extension]
rmImplied e = flip (\\) implied
  where implied = delete e $ expandExtension e

mergeImplied :: [Extension] -> [Extension]
mergeImplied exts = foldl (flip rmImplied) exts exts
