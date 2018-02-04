{-# LANGUAGE FlexibleContexts, LambdaCase, MonoLocalBinds, ScopedTypeVariables, TypeApplications #-}

module Language.Haskell.Tools.Refactor.Builtin.FloatOut
  (floatOut, floatOutRefactoring) where

import Control.Monad.State
import Control.Reference
import Data.List
import Data.Maybe (Maybe(..), catMaybes)

import Language.Haskell.Tools.Refactor

import Name as GHC (Name, NamedThing(..), occNameString)
import SrcLoc (RealSrcSpan)

floatOutRefactoring :: RefactoringChoice
floatOutRefactoring = SelectionRefactoring "FloatOut" (localRefactoring . floatOut)

floatOut :: RealSrcSpan -> LocalRefactoring
floatOut sp mod
  = do (mod', st) <- runStateT (nodesContaining sp !~ extractAndInsert sp $ mod) NotEncountered
       case st of NotEncountered -> refactError "No definition is selected. The selection range must be inside the definition."
                  Extracted bnds -> -- insert it to the global definition list
                                    return $ modDecl & annListElems .- (++ map toTopLevel bnds) $ removeEmpties mod'
                  Inserted -> -- already inserted to a local scope
                               return (removeEmpties mod')
  where toTopLevel :: LocalBind -> Decl
        toTopLevel (LocalValBind vb) = mkValueBinding vb
        toTopLevel (LocalTypeSig sg) = mkTypeSigDecl sg
        toTopLevel (LocalFixity fx) = mkFixityDecl fx

        removeEmpties = removeEmptyBnds (nodesContaining sp) (nodesContaining sp)

data FloatState = NotEncountered | Extracted [LocalBind] | Inserted

extractAndInsert :: RealSrcSpan -> LocalBindList -> StateT FloatState LocalRefactor LocalBindList
extractAndInsert sp locs
  | hasSharedSig = refactError "Cannot float out a definition, since it has a signature shared with other bindings that stay in the scope."
  | not (null nameConflicts) = refactError $ "Cannot float out a definition, since it would cause a name conflicts in the target scope: "
                                                ++ concat (intersperse ", " nameConflicts)
  | not (null implicitConflicts) = refactError $ "Cannot float out a definition, since it uses the implicit parameters: "
                                                   ++ concat (intersperse ", " implicitConflicts)
  | otherwise = get >>= \case NotEncountered -> put (Extracted floated) >> return filteredLocs
                              Extracted binds -> put Inserted >> (return $ annListElems .- (++ binds) $ locs)
                              Inserted -> return locs
  where selected = locs ^? annList & filtered (isInside sp)
        floated = normalizeElements $ selected ++ (locs ^? annList & filtered (nameIsSelected . map semanticsName . (^? elementName)))
          where nameIsSelected [n] = n `elem` concatMap (map semanticsName . (^? elementName)) selected
                nameIsSelected _ = False

        filteredLocs = filterList (\e -> not (getRange e `elem` floatedElemRanges)) locs
          where floatedElemRanges = map getRange floated
        hasSharedSig = any (\e -> not $ null (map semanticsName (filteredLocs ^? annList & elementName) `intersect` map semanticsName (e ^? elementName))) selected
        conflicts = map checkConflict selected

        nameConflicts = concat $ map fst conflicts
        implicitConflicts = concat $ map snd conflicts

checkConflict :: LocalBind -> ([String], [String])
checkConflict bnd = (concatMap @[] getConflict bndNames, implicits)
  where bndNames = bnd ^? elementName
        getConflict bndName = filter ((== nameStr) . Just) $ map (occNameString . getOccName) outerScope
          where outerScope = map (^. _1) $ concat $ take 1 $ drop 2 $ semanticsScope bndName
                nameStr = fmap (occNameString . getOccName) $ semanticsName bndName
        implicits = map (occNameString . getOccName)
                        (concatMap getPossibleImplicits bndNames `intersect` getQNames (bnd ^? biplateRef))

        getQNames :: [QualifiedName] -> [GHC.Name]
        getQNames = catMaybes . map semanticsName

        getPossibleImplicits :: QualifiedName -> [GHC.Name]
        getPossibleImplicits qn = concat (map (map (^. _1)) $ take 2 $ semanticsScope qn) \\ catMaybes (map semanticsName bndNames)
