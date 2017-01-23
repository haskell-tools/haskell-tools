{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Refactor.Predefined.FloatOut where

import Control.Monad.State
import Control.Reference
import Data.Function (on)
import Data.List

import Language.Haskell.Tools.Refactor

import SrcLoc

type FloatOutDefinition dom = HasNameInfo dom

floatOut :: FloatOutDefinition dom => RealSrcSpan -> LocalRefactoring dom
floatOut sp mod 
  = let (mod', st) = runState (nodesContaining sp !~ extractAndInsert sp $ mod) NotEncountered
     in case st of NotEncountered -> refactError "No definition is selected."
                   Extracted bnds -> -- insert it to the global definition list
                                     return $ modDecl & annListElems .- (++ map toTopLevel bnds) $ removeEmpties mod'
                   Inserted -> -- already inserted to a local scope
                               return (removeEmpties mod')
  where toTopLevel :: LocalBind dom -> Decl dom
        toTopLevel (LocalValBind vb) = mkValueBinding vb
        toTopLevel (LocalTypeSig sg) = mkTypeSigDecl sg
        toTopLevel (LocalFixity fx) = mkFixityDecl fx

        removeEmpties = removeEmptyBnds (nodesContaining sp) (nodesContaining sp)

data FloatState dom = NotEncountered | Extracted [LocalBind dom] | Inserted

extractAndInsert :: FloatOutDefinition dom => RealSrcSpan -> LocalBindList dom -> State (FloatState dom) (LocalBindList dom)
extractAndInsert sp locs = get >>= \case NotEncountered -> put (Extracted floated) >> return filteredLocs
                                         Extracted binds -> put Inserted >> (return $ annListElems .- (++ binds) $ locs)
                                         Inserted -> return locs
  where selected = locs ^? annList & filtered (isInside sp)
        floated = normalizeElements $ selected ++ (locs ^? annList & filtered (nameIsSelected . (^? elementName)))
          where nameIsSelected [n] = n `elem` concatMap (^? elementName) selected 
                nameIsSelected _ = False

        filteredLocs = filterList (\e -> not (getRange e `elem` floatedElemRanges)) locs
          where floatedElemRanges = map getRange floated

