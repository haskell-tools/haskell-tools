{-# LANGUAGE LambdaCase
           , ConstraintKinds 
           #-}
module Language.Haskell.Tools.Refactor.Predefined.FloatOut where

import Control.Monad.State
import Control.Reference

import Language.Haskell.Tools.Refactor

import SrcLoc

type FloatOutDefinition dom = Domain dom

floatOut :: FloatOutDefinition dom => RealSrcSpan -> LocalRefactoring dom
floatOut sp mod 
  = let (mod', st) = runState (nodesContaining sp !~ extractAndInsert sp $ mod) NotEncountered
     in case st of NotEncountered -> refactError "No definition is selected."
                   Extracted bnds -> -- insert it to the global definition list
                                     return $ modDecl & annListElems .- (++ map toTopLevel bnds) $ mod'
                   Inserted -> -- already inserted to a local scope
                               return mod'
  where toTopLevel :: LocalBind dom -> Decl dom
        toTopLevel (LocalValBind vb) = mkValueBinding vb
        toTopLevel (LocalTypeSig sg) = mkTypeSigDecl sg
        toTopLevel (LocalFixity fx) = mkFixityDecl fx

data FloatState dom = NotEncountered | Extracted [LocalBind dom] | Inserted

extractAndInsert :: RealSrcSpan -> LocalBindList dom -> State (FloatState dom) (LocalBindList dom)
extractAndInsert sp locs = get >>= \case NotEncountered -> put (Extracted floated) >> return filteredLocs
                                         Extracted binds -> put Inserted >> (return $ annListElems .- (++ binds) $ locs)
  where floated = locs ^? annList & filtered (isInside sp)
        filteredLocs = filterList (not . isInside sp) locs