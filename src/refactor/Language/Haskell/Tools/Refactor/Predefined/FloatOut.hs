{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           , ViewPatterns
           , TypeApplications
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.Refactor.Predefined.FloatOut where

import Control.Monad.State
import Control.Reference
import Data.List
import Data.Maybe (Maybe(..), catMaybes)

import Language.Haskell.Tools.Refactor

import Name as GHC (Name, NamedThing(..), occNameString)
import OccName (occNameString)
import SrcLoc (RealSrcSpan)

type FloatOutDefinition dom = (HasNameInfo dom, HasScopeInfo dom)

floatOut :: FloatOutDefinition dom => RealSrcSpan -> LocalRefactoring dom
floatOut sp mod
  = do (mod', st) <- runStateT (nodesContaining sp !~ extractAndInsert sp $ mod) NotEncountered
       case st of NotEncountered -> refactError "No definition is selected. The selection range must be inside the definition."
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
  | not (null nameConflicts) = refactError $ "Cannot float out a definition, since it would cause a name conflicts in the target scope: "
                                                ++ concat (intersperse ", " nameConflicts)
  | not (null implicitConflicts) = refactError $ "Cannot float out a definition, since it uses the implicit parameters: "
                                                   ++ concat (intersperse ", " implicitConflicts)
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
        conflicts = map checkConflict selected

        nameConflicts = concat $ map fst conflicts
        implicitConflicts = concat $ map snd conflicts

checkConflict :: forall dom . FloatOutDefinition dom => LocalBind dom -> ([String], [String])
checkConflict bnd = (concatMap @[] getConflict bndNames, implicits)
  where bndNames = bnd ^? elementName
        getConflict bndName = filter ((== nameStr) . Just) $ map (occNameString . getOccName) outerScope
          where outerScope = map (^. _1) $ concat $ take 1 $ drop 2 $ semanticsScope bndName
                nameStr = fmap (occNameString . getOccName) $ semanticsName bndName
        implicits = map (occNameString . getOccName)
                        (concatMap getPossibleImplicits bndNames `intersect` getQNames (bnd ^? biplateRef))

        getQNames :: [QualifiedName dom] -> [GHC.Name]
        getQNames = catMaybes . map semanticsName

        getPossibleImplicits :: QualifiedName dom -> [GHC.Name]
        getPossibleImplicits qn = concat (map (map (^. _1)) $ take 2 $ semanticsScope qn) \\ catMaybes (map semanticsName bndNames)
