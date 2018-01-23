{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
            #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.GHCTypeTraversal.Checkable where

import Var
import TyCon
import TyCoRep
import CoAxiom

import PrelNames (eqTyConName)
import Unique (hasKey, getUnique)

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.ExtMonadMisc

import Debug.Trace

instance Checkable Type where
  check = traceShow "TYPE" . return

instance Checkable Var where
  check = traceShow "VAR" . return

instance Checkable TyCon where
  check tc
    | tc `hasKey` getUnique eqTyConName = traceShow "TYCON" $ addOccurenceNoLoc TypeFamilies >> return tc
    | otherwise                         = traceShow "TYCON" $ return tc

instance Checkable Coercion where
  check = return

instance Checkable UnivCoProvenance where
  check = return
