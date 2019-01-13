{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap
  , module Language.Haskell.TH.LanguageExtensions
  , Formula(..)
  ) where

import GHC.Generics
import Control.DeepSeq

import SrcLoc (SrcSpan)

import Data.List
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

prettyPrintFormula :: Show a => LogicalRelation a -> String
prettyPrintFormula (Var x) = show x
prettyPrintFormula (x :||: y) = prettyPrintFormula x ++ " OR " ++ prettyPrintFormula y
prettyPrintFormula (x :&&: y) = prettyPrintFormula x ++ " AND " ++ prettyPrintFormula y
prettyPrintFormula (x :++: y) = prettyPrintFormula x ++ " XOR " ++ prettyPrintFormula y
prettyPrintFormula (x :->: y) = prettyPrintFormula x ++ " => " ++ prettyPrintFormula y
prettyPrintFormula (x :<->: y) = prettyPrintFormula x ++ " <=> " ++ prettyPrintFormula y
prettyPrintFormula (All xs) = "All [" ++ (intercalate ", " . map prettyPrintFormula $ xs) ++ "]"
prettyPrintFormula (Some xs) = "Some [" ++ (intercalate ", " . map prettyPrintFormula $ xs) ++ "]"
prettyPrintFormula (None xs) = "None [" ++ (intercalate ", " . map prettyPrintFormula $ xs) ++ "]"
prettyPrintFormula (ExactlyOne xs) = "ExactlyOne [" ++ (intercalate ", " . map prettyPrintFormula $ xs) ++ "]"
prettyPrintFormula (AtMostOne xs) = "AtMostOne [" ++ (intercalate ", " . map prettyPrintFormula $ xs) ++ "]"
prettyPrintFormula (Not x) = "Not " ++ prettyPrintFormula x
prettyPrintFormula Yes = "True"
prettyPrintFormula No = "False"
prettyPrintFormula Let{} = "Let ..."
prettyPrintFormula Bound{} = error "Bound is for internal use only"

data Occurence a = Hint               { unOcc :: a }
                 | Evidence           { unOcc :: a }
                 | MissingInformation { unOcc :: a }
  deriving (Show, Eq, Ord, Functor)

type ExtMap = SMap.Map (LogicalRelation Extension) [Occurence SrcSpan]

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
complexity = length . expandExtension'

-- | Completely expand extension
expandExtension' :: Extension -> [Extension]
expandExtension' e = findFixedPoint (nub . expand) [e]
  where expand = concatMap expandExtension

-- | Terminating variant of Control.Monad.Fix.fix
-- It tries to find the fixpoint of a function with a given starting value.
-- It terminates if a value repeats itself.
findFixedPoint :: Eq a => (a -> a) -> a -> a
findFixedPoint f x = findFix' f (f x) x
  where findFix' f cur prev = if cur == prev then cur else findFix' f (f cur) cur

determineExtensions :: SMap.Map (LogicalRelation Extension) v -> [Extension]
{- NOTE:
 We calculate all possible extension sets that satisfy the logical relation,
 then for each extension we remove all the extensions implied by it, (mergeImplied)
 finally we select the minimal extension set.

 An extension set is minimal if:
  - it contains the least amount of extensions
  - it contains extension with minimal complexity
  - it is lexicographically the first one (based on the Ord instance of Extension)

  TODO: Comparing on length might be unnecessary
-}
determineExtensions x = minimal . map toExts $ solution
  where solution = solve_all . All . LMap.keys $ x
        toExts   = mergeImplied . map fst . filter snd . LMap.toList
        minimal  = minimumBy ((compare `on` length) <> (compare `on` (sum . map complexity)) <> (compare `on` sort))

rmImplied :: Extension -> [Extension] -> [Extension]
rmImplied e = flip (\\) implied
  where implied = delete e . expandExtension' $ e

mergeImplied :: [Extension] -> [Extension]
mergeImplied exts = foldl (flip rmImplied) exts exts
