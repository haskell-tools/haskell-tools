{-# LANGUAGE DeriveFunctor
           , DeriveGeneric
           , DeriveAnyClass
           #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap where

import GHC.Generics
import Control.DeepSeq

import Language.Haskell.TH.LanguageExtensions (Extension)
import SrcLoc (SrcSpan)

import qualified Data.Map.Strict as SMap (Map)


infix 6 :||:
infix 7 :&&:

data LogicalRelation a = LVar a
                       | Not (LogicalRelation a)
                       | LogicalRelation a :&&: LogicalRelation a
                       | LogicalRelation a :||: LogicalRelation a
  deriving (Eq, Show, Functor, Ord, Generic, NFData)

type ExtMap = SMap.Map (LogicalRelation Extension) [SrcSpan]

instance NFData Extension where
  rnf x = seq x ()
