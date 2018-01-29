{-# LANGUAGE FlexibleContexts
           , LambdaCase
           , ScopedTypeVariables
           , TupleSections
           , TypeApplications
           , TypeFamilies #-}
module Language.Haskell.Tools.Refactor.Builtin.OrganizeImports
  ( organizeImports, projectOrganizeImports
  , organizeImportsRefactoring, projectOrganizeImportsRefactoring
  ) where

import ConLike (ConLike(..))
import DataCon (dataConTyCon)
import DynFlags (xopt)
import FamInstEnv (FamInst(..))
import GHC (TyThing(..), lookupName)
import qualified GHC
import Id
import IdInfo (RecSelParent(..))
import InstEnv (ClsInst(..))
import Language.Haskell.TH.LanguageExtensions as GHC (Extension(..))
import Name (NamedThing(..))
import OccName (HasOccName(..), isSymOcc)
import qualified PrelNames as GHC (fromStringName, coerceKey)
import SrcLoc (SrcSpan(..), noSrcSpan)
import TyCon (TyCon(..), tyConFamInst_maybe)
import Unique (getUnique)
import CoreSyn as GHC (isOrphan)

import Control.Applicative ((<$>), Alternative(..))
import Control.Monad
import Control.Reference hiding (element)
import Data.Function hiding ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List
import Data.Maybe (Maybe(..), maybe, catMaybes)

import Language.Haskell.Tools.Refactor as AST

organizeImportsRefactoring :: RefactoringChoice
organizeImportsRefactoring = ModuleRefactoring "OrganizeImports" (localRefactoring organizeImports)

projectOrganizeImportsRefactoring :: RefactoringChoice
projectOrganizeImportsRefactoring = ProjectRefactoring "ProjectOrganizeImports" projectOrganizeImports

projectOrganizeImports :: ProjectRefactoring
projectOrganizeImports mods
  = mapM (\(k, m) -> ContentChanged . (k,) <$> localRefactoringRes id m (organizeImports m)) mods

organizeImports :: LocalRefactoring
organizeImports mod
  = do usedTyThings <- catMaybes <$> mapM lookupName usedNames
       let dfs = semanticsDynFlags mod
           noNarrowingImports
             = xopt TemplateHaskell dfs -- no narrowing if TH is present (we don't know what will be used)
                || xopt QuasiQuotes dfs -- no narrowing if TH quotes are present (we don't know what will be used)
                || (xopt FlexibleInstances dfs && noNarrowingSubspecs) -- arbitrary ctors might be needed when using imported data families
                || hasCoerce -- the presence of coerce implicitely requires the constructors to be present
           noNarrowingSubspecs
             = -- both standalone deriving and FFI marshalling can cause constructors to be used in the generated code
               xopt GHC.StandaloneDeriving dfs || hasMarshalling
                   -- while pattern synonyms are not handled correctly, we disable subspec narrowing to be safe
                || patternSynonymAreUsed usedTyThings
       if noNarrowingImports
         then -- we don't know what definitions the generated code will use
              return $ modImports .- sortImports $ mod
         else do (prelInstances, prelFamInsts) <- liftGhc $ getInstances preludeAccessible
                 modImports !~ fmap sortImports . narrowImports noNarrowingSubspecs exportedModules (addFromString dfs usedNames) exportedNames prelInstances prelFamInsts $ mod
  where preludeAccessible = semanticsPrelTransMods mod
        addFromString dfs = if xopt OverloadedStrings dfs then (GHC.fromStringName :) else id
        usedNames = map getName $ (catMaybes $ map semanticsName
                        -- obviously we don't want the names in the imports to be considered, but both from
                        -- the declarations (used), both from the module head (re-exported) will count as usage
                      (universeBi (mod ^. modHead) ++ universeBi (mod ^. modDecl) :: [QualifiedName]))
                        ++ concatMap (map fst . semanticsImplicitFlds) (universeBi (mod ^. modDecl) :: [FieldWildcard])
        -- Prelude is not actually exported, but we don't want to remove it if it is explicitly there
        -- otherwise, we might add new imported elements that cause conflicts.
        exportedModules = "Prelude" : (mod ^? modHead & annJust & mhExports & annJust
                                                & espExports & annList & exportModuleName & moduleNameString)

        -- Exported definitions must be kept imported
        exports = mod ^? modHead & annJust & mhExports & annJust & espExports & annList & exportDecl
        exportedNames = catMaybes $ map getExported exports
        getExported e = fmap (,hasChild) name
          where name = semanticsName (e ^. ieName & simpleName)
                hasChild = (case e ^? ieSubspec & annJust of Just SubAll -> True; _ -> False)
                             || not (null @[] (e ^? ieSubspec & annJust & essList & annList))

        -- Checks if the module uses foreign import/export that requires marshalling. In this case no
        -- subspecifiers could be narrowed because constructors might be needed.
        hasMarshalling = not $ null @[] (mod ^? modDecl & annList & declForeignType)
        hasCoerce = GHC.coerceKey `elem` map getUnique usedNames

        -- Can this name be part of the source code?
        -- isSourceName n = ':' `notElem` occNameString (occName n)
        patternSynonymAreUsed tts = any (\case AConLike (PatSynCon _) -> True; _ -> False) tts

-- | Sorts the imports in alphabetical order
sortImports :: ImportDeclList -> ImportDeclList
sortImports ls = srcInfo & srcTmpSeparators .= filter (not . null . fst) (concatMap (\(sep,elems) -> sep : map fst elems) reordered)
                   $ annListElems .= concatMap (map snd . snd) reordered
                   $ ls
  where reordered :: [(([SourceTemplateTextElem], SrcSpan), [(([SourceTemplateTextElem], SrcSpan), ImportDecl)])]
        reordered = map (_2 .- sortBy (compare `on` (^. _2 & importModule & AST.moduleNameString))) parts

        parts = map (_2 .- reverse) $ reverse $ breakApart [] imports

        -- break up the list of imports to import groups
        breakApart :: [(([SourceTemplateTextElem], SrcSpan), [(([SourceTemplateTextElem], SrcSpan), ImportDecl)])]
                        -> [(([SourceTemplateTextElem], SrcSpan), ImportDecl)]
                        -> [(([SourceTemplateTextElem], SrcSpan), [(([SourceTemplateTextElem], SrcSpan), ImportDecl)])]
        breakApart res [] = res
        breakApart res ((sep, e) : rest) | length (filter ('\n' ==) (sep ^? _1 & traversal & sourceTemplateText & traversal)) > 1
                                            || "\n#" `isInfixOf` (sep ^? _1 & traversal & sourceTemplateText & traversal)
          = breakApart ((sep, [(([], noSrcSpan),e)]) : res) rest
        breakApart ((lastSep, lastRes) : res) (elem : rest)
          = breakApart ((lastSep, elem : lastRes) : res) rest
        breakApart [] ((sep, e) : rest)
          = breakApart [(sep, [(([], noSrcSpan),e)])] rest

        imports = zipWithSeparators ls

-- | Modify an import to only import  names that are used.
narrowImports :: Bool -> [String] -> [GHC.Name] -> [(GHC.Name, Bool)] -> [ClsInst] -> [FamInst] -> ImportDeclList -> LocalRefactor ImportDeclList
narrowImports noNarrowSubspecs exportedModules usedNames exportedNames prelInsts prelFamInsts imps
  = do impsNeeded <- liftGhc $ neededImports exportedModules (usedNames ++ map fst exportedNames) prelInsts prelFamInsts (imps ^. annListElems)
       (annListElems & traversal !~ narrowImport noNarrowSubspecs exportedModules usedNames exportedNames)
          =<< filterListIndexedSt (\i _ -> impsNeeded !! i) imps

-- | Reduces the number of definitions used from an import
narrowImport :: Bool -> [String] -> [GHC.Name] -> [(GHC.Name, Bool)] -> ImportDecl -> LocalRefactor ImportDecl
narrowImport noNarrowSubspecs exportedModules usedNames exportedNames imp
  | (imp ^. importModule & moduleNameString) `elem` exportedModules
      || maybe False (`elem` exportedModules) (imp ^? importAs & annJust & importRename & moduleNameString)
  = return imp -- dont change an import if it is exported as-is (module export)
  | importIsExact imp
  = importSpec&annJust&importSpecList !~ narrowImportSpecs noNarrowSubspecs usedNames exportedNames $ imp
  | importIsHiding imp
  = return imp -- a hiding import is not changed, because the wildcard importing of class and datatype
               -- members could bring into scope the exact definition that was hidden
  | otherwise
  = do namedThings <- mapM lookupName actuallyImported
       let -- to explicitly import pattern synonyms or type operators we need to enable an extension, and the user might not expect this
           hasRiskyDef = any isRiskyDef namedThings
           groups = groupThings noNarrowSubspecs (semanticsImported imp)
                      (filter ((`elem` semanticsImported imp) . fst) exportedNames) (catMaybes namedThings)
       return $ if not hasRiskyDef && length groups < 4
         then importSpec .- replaceWithJust (createImportSpec groups) $ imp
         else imp
  where actuallyImported = semanticsImported imp `intersect` usedNames
        isRiskyDef (Just (AConLike (PatSynCon _))) = True
        isRiskyDef (Just (ATyCon tc)) = isSymOcc (occName (tyConName tc))
        isRiskyDef _ = False

-- | Group things as importable definitions. The second member of the pair will be true, when there is a sub-name
-- that should be imported apart from the name of the importable definition.
groupThings :: Bool -> [GHC.Name] -> [(GHC.Name, Bool)] -> [TyThing] -> [(GHC.Name, Bool)]
groupThings noNarrowSubspecs importable exported
  = map last . groupBy ((==) `on` fst) . sort . (exported ++) . map createImportFromTyThing
  where createImportFromTyThing :: TyThing -> (GHC.Name, Bool)
        createImportFromTyThing tt | Just (td, isDataType) <- getTopDef tt
          = if (td `elem` importable || isDataType) then (td, True)
                                                    else (getName tt, False)
        createImportFromTyThing tt@(ATyCon {}) = (getName tt, noNarrowSubspecs)
        createImportFromTyThing tt = (getName tt, False)

-- | Gets the importable definition for a (looked up) name. The bool flag tells if it is from
-- a data type.
getTopDef :: TyThing -> Maybe (GHC.Name, Bool)
getTopDef (AnId id) | isRecordSelector id
  = case recordSelectorTyCon id of RecSelData tc -> Just (getName tc, True)
                                   RecSelPatSyn ps -> Just (getName ps, False)
getTopDef (AnId id)
  | Just n <- fmap (getName . dataConTyCon) (isDataConWorkId_maybe id <|> isDataConId_maybe id)
  = Just (n, True)
getTopDef (AnId id) = fmap ((,False) . getName) (isClassOpId_maybe id)
getTopDef (AConLike (RealDataCon dc))
  = case tyConFamInst_maybe (dataConTyCon dc) of
      Just (dataFam, _) -> Just (getName dataFam, True)
      _                 -> Just (getName $ dataConTyCon dc, True)
getTopDef (AConLike (PatSynCon _)) = error "getTopDef: should not be called with pattern synonyms"
getTopDef (ATyCon _) = Nothing

createImportSpec :: [(GHC.Name, Bool)] -> ImportSpec
createImportSpec elems = mkImportSpecList $ map createIESpec elems
  where createIESpec (n, False) = mkIESpec (mkUnqualName' (GHC.getName n)) Nothing
        createIESpec (n, True)  = mkIESpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

-- | Check each import if it is actually needed
neededImports :: [String] -> [GHC.Name] -> [ClsInst] -> [FamInst] -> [ImportDecl] -> GHC.Ghc [Bool]
neededImports exportedModules usedNames prelInsts prelFamInsts imps = do
     impsWithInsts <- mapM (\i -> (i,) <$> getInstances (semanticsTransMods i)) imps  
     return $ neededImports' usedNames [] prelInsts prelFamInsts impsWithInsts
  where neededImports' _ _ _ _ [] = []
        -- keep the import if any definition is needed from it
        neededImports' usedNames kept keptInsts keptFamInsts ((imp, (clsInsts, famInsts)) : rest)
          | not (null actuallyImported)
               || (imp ^. importModule & moduleNameString) `elem` exportedModules
               || maybe False (`elem` exportedModules) (imp ^? importAs & annJust & importRename & moduleNameString)
            = True : neededImports' usedNames (imp : kept) (clsInsts ++ keptInsts) (famInsts ++ keptFamInsts) rest
          where actuallyImported = semanticsImported imp `intersect` usedNames
        -- check if instances are needed from the import
        neededImports' usedNames kept keptInsts keptFamInsts ((imp, (clsInsts, famInsts)) : rest)
            = needed : if needed then neededImports' usedNames (imp : kept) (clsInsts ++ keptInsts) (famInsts ++ keptFamInsts) rest
                                 else neededImports' usedNames kept keptInsts keptFamInsts rest
          where needed = any (`notElem` map is_dfun keptInsts) (map is_dfun $ filter (isOrphan . is_orphan) clsInsts)
                           || any (`notElem` map fi_axiom keptFamInsts) (map fi_axiom famInsts)

-- | Narrows the import specification (explicitly imported elements)
narrowImportSpecs :: Bool -> [GHC.Name] -> [(GHC.Name, Bool)] -> IESpecList -> LocalRefactor IESpecList
narrowImportSpecs noNarrowSubspecs usedNames exportedNames
  = (if noNarrowSubspecs then return else annList !~ narrowImportSubspecs neededNames exportedNames)
       >=> filterListSt isNeededSpec
  where neededNames = usedNames ++ map fst exportedNames

        isNeededSpec :: IESpec -> Bool
        isNeededSpec ie =
          (semanticsName (ie ^. ieName&simpleName)) `elem` map Just neededNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? ieSubspec&annJust&essList&annList) /= [])
            -- its a bit hard to decide if there is an element inside (for example bundled pattern synonyms)
            || (case ie ^? ieSubspec&annJust of Just SubAll -> True; _ -> False)

-- | Reduces the number of definitions imported from a sub-specifier.
narrowImportSubspecs :: [GHC.Name] -> [(GHC.Name, Bool)] -> IESpec -> LocalRefactor IESpec
narrowImportSubspecs neededNames exportedNames ss | noNarrowingForThis = return ss
  | otherwise
  = ieSubspec & annJust & essList !~ filterListSt (\n -> (semanticsName (n ^. simpleName)) `elem` map Just neededNames) $ ss
  where noNarrowingForThis = case semanticsName (ss ^. ieName&simpleName) of
                               Just name -> lookup name exportedNames == Just True
                               _ -> False
