{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           , ViewPatterns
           , TypeApplications
           #-}
module Language.Haskell.Tools.Refactor.Predefined.FloatOut where

import Control.Monad.State
import Control.Reference
import Data.Function (on)
import Data.List
import Data.Maybe

import Language.Haskell.Tools.Refactor

import Name
import OccName
import SrcLoc

type FloatOutDefinition dom = (HasNameInfo dom, HasScopeInfo dom)

floatOut :: FloatOutDefinition dom => RealSrcSpan -> LocalRefactoring dom
floatOut sp mod 
  = do (mod', st) <- runStateT (nodesContaining sp !~ extractAndInsert sp $ mod) NotEncountered
       case st of NotEncountered -> refactError "No definition is selected."
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

extractAndInsert :: FloatOutDefinition dom => RealSrcSpan -> LocalBindList dom -> StateT (FloatState dom) (LocalRefactor dom) (LocalBindList dom)
extractAndInsert sp locs 
  | hasSharedSig = refactError "Cannot float out a definition, since it has a signature shared with other bindings that stay in the scope."
  | hasConflict = refactError "Cannot float out a definition, since it would cause a name conflict in the target scope."
  | otherwise = get >>= \case NotEncountered -> put (Extracted floated) >> return filteredLocs
                              Extracted binds -> put Inserted >> (return $ annListElems .- (++ binds) $ locs)
                              Inserted -> return locs
  where selected = locs ^? annList & filtered (isInside sp)
        floated = normalizeElements $ selected ++ (locs ^? annList & filtered (nameIsSelected . (^? elementName)))
          where nameIsSelected [n] = n `elem` concatMap (^? elementName) selected 
                nameIsSelected _ = False

        filteredLocs = filterList (\e -> not (getRange e `elem` floatedElemRanges)) locs
          where floatedElemRanges = map getRange floated
        hasSharedSig = any (\e -> not $ null ((filteredLocs ^? annList & elementName) `intersect` (e ^? elementName))) selected
        hasConflict = any checkConflict selected

checkConflict :: FloatOutDefinition dom => LocalBind dom -> Bool
checkConflict ((^? elementName) -> bndNames) = any @[] hasConflict bndNames
  where hasConflict bndName = isJust $ find ((== nameStr) . Just . occNameString . getOccName) outerScope
          where outerScope = head $ tail $ semanticsScope bndName 
                nameStr = fmap (occNameString . getOccName) $ semanticsName bndName
