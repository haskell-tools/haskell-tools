{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.RenameDefinition
  (renameDefinition, renameDefinition', renameDefinitionRefactoring) where

import DataCon (dataConFieldLabels, FieldLbl(..), dataConFieldType)
import qualified GHC
import Id
import IdInfo (RecSelParent(..))
import Name (OccName(..), NamedThing(..), occNameString)
import SrcLoc (RealSrcSpan)
import TyCon (tyConDataCons)
import Type (funResultTy, eqType)

import Control.Monad.State
import Control.Reference as Ref
import Data.Generics.Uniplate.Data ()
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Language.Haskell.Tools.Refactor

renameDefinitionRefactoring :: RefactoringChoice
renameDefinitionRefactoring = NamingRefactoring "RenameDefinition" renameDefinition'

renameDefinition' :: RealSrcSpan -> String -> Refactoring
renameDefinition' sp str mod mods
  = case (getNodeContaining sp (snd mod) :: Maybe QualifiedName) >>= (fmap getName . semanticsName) of
      Just name -> do let sameNames = bindsWithSameName name (snd mod ^? biplateRef)
                      renameDefinition name sameNames str mod mods
        where bindsWithSameName :: GHC.Name -> [FieldWildcard] -> [GHC.Name]
              bindsWithSameName name wcs = catMaybes $ map ((lookup name) . semanticsImplicitFlds) wcs
      Nothing -> case getNodeContaining sp (snd mod) of
                   Just modName -> renameModule (any @[] (sp `isInside`) ((snd mod) ^? modImports&annList&importAs))
                                                (modName ^. moduleNameString) str mod mods
                   Nothing -> refactError "No name is selected"

renameModule :: Bool -> String -> String -> Refactoring
renameModule isAlias from to m mods
    | any (nameConflict to) (map snd $ m:mods) = refactError "Name conflict when renaming module"
    | isJust (validModuleName to) = refactError $ "The given name is not a valid module name: " ++ fromJust (validModuleName to)
    | otherwise = -- here it is important that the delete is the last, because rename
                  -- can still use the info about the deleted module
                  (if isAlias then id else (fmap (\ls -> map (alterChange from to) ls ++ [ModuleRemoved from])))
                    $ mapM (\(name,mod) -> ContentChanged . (name,) <$> localRefactoringRes id mod (replaceModuleNames =<< alterNormalNames mod)) (m:mods)
  where alterChange from to (ContentChanged (mod,res))
          | (mod ^. sfkModuleName) == from
          = ModuleCreated to res mod
        alterChange _ _ c = c

        replaceModuleNames :: LocalRefactoring
        replaceModuleNames = modNames & filtered (\e -> (e ^. moduleNameString) == from) != mkModuleName to
          where modNames = modHead & annJust & (mhName &+& mhExports & annJust & espExports & annList & exportModuleName)
                             &+& modImports & annList & ( importModule
                                                            &+& importAs & annJust & importRename )

        alterNormalNames :: LocalRefactoring
        alterNormalNames mod =
           biplateRef @_ @QualifiedName & filtered (\e -> concat (intersperse "." (e ^? qualifiers&annList&simpleNameStr)) == from)
             !- (\e -> mkQualifiedName (splitOn "." to) (e ^. unqualifiedName&simpleNameStr)) $ mod

        nameConflict :: String -> Module -> Bool
        nameConflict to mod
          = let modName = mod ^? modHead&annJust&mhName&moduleNameString
                imports = mod ^? modImports&annList
                importNames = map (\imp -> fromMaybe (imp ^. importModule) (imp ^? importAs&annJust&importRename) ^. moduleNameString) imports
             in modName == Just to || to `elem` importNames

renameDefinition :: GHC.Name -> [GHC.Name] -> String -> Refactoring
renameDefinition toChangeOrig toChangeWith newName mod mods
    = do nameCls <- classifyName toChangeOrig
         (changedModules,defFound) <- runStateT (catMaybes <$> mapM (renameInAModule toChangeOrig toChangeWith newName) (mod:mods)) False
         if | isJust (nameValid nameCls newName) -> refactError $ "The new name is not valid: " ++ fromJust (nameValid nameCls newName)
            | not defFound -> refactError "The definition to rename was not found. Maybe it is in another package."
            | otherwise -> return $ map ContentChanged changedModules
  where
    renameInAModule :: GHC.Name -> [GHC.Name] -> String -> ModuleDom -> StateT Bool Refactor (Maybe ModuleDom)
    renameInAModule toChangeOrig toChangeWith newName (name, mod)
      = mapStateT (localRefactoringRes (\f (a,s) -> (fmap (\(n,r) -> (n, f r)) a,s)) mod) $
          do origTT <- GHC.lookupName toChangeOrig
             let origId = case origTT of
                            Just (GHC.AnId id) -> Just id
                            _ -> Nothing
             (res, isChanged) <- runStateT (biplateRef !~ changeName toChangeOrig origId toChangeWith newName $ mod) False
             if isChanged then return $ Just (name, res)
                          else return Nothing

    changeName :: GHC.Name -> Maybe Id -> [GHC.Name] -> String -> QualifiedName
                           -> StateT Bool (StateT Bool LocalRefactor) QualifiedName
    changeName toChangeOrig origId toChangeWith str name
      | maybe False (`elem` toChange) actualName
          && semanticsDefining name == False
          && any @[] (\n -> str == occNameString (getOccName (n ^. _1)) && not (mergeableFields origId (n ^. _1))
                              && notQualified (n ^. _2))
                     (scopeUpToDef (semanticsScope name) ^? traversal & traversal & filtered (sameNamespace toChangeOrig . (^. _1)))
      = refactError $ "The definition clashes with an existing one at: " ++ shortShowSpanWithFile (getRange name) -- name clash with an external definition
      | maybe False (`elem` toChange) actualName
      = do put True -- state that something is changed in the local state
           when (actualName == Just toChangeOrig)
             $ lift $ modify (|| semanticsDefining name) -- state that the definition is renamed in the global state
           return $ unqualifiedName .= mkNamePart str $ name -- found the changed name (or a name that have to be changed too)
      | let namesInScope = map (map (^. _1)) $ semanticsScope name
         in case semanticsName name of
              Just (getName -> exprName) -> str == occNameString (getOccName exprName)
                                              && sameNamespace toChangeOrig exprName
                                              && conflicts toChangeOrig exprName namesInScope
                                              && not (mergeableFields origId exprName)
              Nothing -> False -- ambiguous names
      = refactError $ "The definition clashes with an existing one: " ++ shortShowSpanWithFile (getRange name) -- local name clash
      | otherwise = return name -- not the changed name, leave as before
      where toChange = toChangeOrig : toChangeWith
            actualName = fmap getName (semanticsName name)
            scopeUpToDef sc = let (inside, outside) = span (null . (toChange `intersect`) . map (^. _1)) sc
                               in inside ++ take 1 outside
            mergeableFields (Just orig) conflict
              | isRecordSelector orig
              , RecSelData tc <- recordSelectorTyCon orig
              = let selectorsWithTypes = concatMap (\dc -> map (\fld -> (flSelector fld, dataConFieldType dc (flLabel fld))) (dataConFieldLabels dc))
                                                   (filter (\dc -> toChangeOrig `notElem` map flSelector (dataConFieldLabels dc)) (tyConDataCons tc))
                 in maybe False (`eqType` funResultTy (idType orig)) (lookup conflict selectorsWithTypes)
            mergeableFields _ _ = False
            notQualified usage = isNothing usage || any @[] (not . usageQualified) (usage ^? just & traversal)

conflicts :: GHC.Name -> GHC.Name -> [[GHC.Name]] -> Bool
conflicts overwrites overwritten (scopeBlock : scope)
  | overwritten `elem` scopeBlock && overwrites `notElem` scopeBlock = False
  | overwrites `elem` scopeBlock = True
  | otherwise = conflicts overwrites overwritten scope
conflicts _ _ [] = False

sameNamespace :: GHC.Name -> GHC.Name -> Bool
sameNamespace n1 n2 = occNameSpace (getOccName n1) == occNameSpace (getOccName n2)
