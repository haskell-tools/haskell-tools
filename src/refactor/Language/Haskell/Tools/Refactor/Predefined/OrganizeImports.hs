{-# LANGUAGE LambdaCase
           , ScopedTypeVariables
           , FlexibleContexts
           , TypeFamilies
           , ConstraintKinds
           , TupleSections
           , TypeApplications
           #-}
module Language.Haskell.Tools.Refactor.Predefined.OrganizeImports (organizeImports, OrganizeImportsDomain, projectOrganizeImports) where

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
import qualified PrelNames as GHC (fromStringName)
import TyCon (TyCon(..), tyConFamInst_maybe)

import Control.Applicative ((<$>), Alternative(..))
import Control.Monad
import Control.Reference hiding (element)
import Data.Function hiding ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List
import Data.Maybe (Maybe(..), maybe, catMaybes)

import Language.Haskell.Tools.Refactor as AST

type OrganizeImportsDomain dom = ( HasNameInfo dom, HasImportInfo dom, HasModuleInfo dom )

projectOrganizeImports :: forall dom . OrganizeImportsDomain dom => Refactoring dom
projectOrganizeImports mod mods
  = mapM (\(k, m) -> ContentChanged . (k,) <$> localRefactoringRes id m (organizeImports m)) (mod:mods)

organizeImports :: forall dom . OrganizeImportsDomain dom => LocalRefactoring dom
organizeImports mod
  = do usedTyThings <- catMaybes <$> mapM lookupName usedNames
       let dfs = semanticsDynFlags mod
           noNarrowingImports
             = xopt TemplateHaskell dfs -- no narrowing if TH is present (we don't know what will be used)
                || xopt QuasiQuotes dfs -- no narrowing if TH quotes are present (we don't know what will be used)
                || (xopt FlexibleInstances dfs && noNarrowingSubspecs) -- arbitrary ctors might be needed when using imported data families
           noNarrowingSubspecs
             = -- both standalone deriving and FFI marshalling can cause constructors to be used in the generated code
               xopt GHC.StandaloneDeriving dfs || hasMarshalling
                   -- while pattern synonyms are not handled correctly, we disable subspec narrowing to be safe
                || patternSynonymAreUsed usedTyThings
       if noNarrowingImports
         then -- we don't know what definitions the generated code will use
              return $ modImports .- sortImports $ mod
         else modImports !~ narrowImports noNarrowingSubspecs exportedModules (addFromString dfs usedNames) exportedNames prelInstances prelFamInsts . sortImports $ mod
  where prelInstances = semanticsPrelOrphanInsts mod
        prelFamInsts = semanticsPrelFamInsts mod
        addFromString dfs = if xopt OverloadedStrings dfs then (GHC.fromStringName :) else id
        usedNames = map getName $ catMaybes $ map semanticsName
                        -- obviously we don't want the names in the imports to be considered, but both from
                        -- the declarations (used), both from the module head (re-exported) will count as usage
                      $ (universeBi (mod ^. modHead) ++ universeBi (mod ^. modDecl) :: [QualifiedName dom])
        -- Prelude is not actually exported, but we don't want to remove it if it is explicitely there
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

        -- Can this name be part of the source code?
        -- isSourceName n = ':' `notElem` occNameString (occName n)
        patternSynonymAreUsed tts = any (\case AConLike (PatSynCon _) -> True; _ -> False) tts

-- | Sorts the imports in alphabetical order
sortImports :: forall dom . ImportDeclList dom -> ImportDeclList dom
sortImports ls = srcInfo & srcTmpSeparators .= filter (not . null) (concatMap (\(sep,elems) -> sep : map fst elems) reordered)
                   $ annListElems .= concatMap (map snd . snd) reordered
                   $ ls
  where reordered :: [(String, [(String, ImportDecl dom)])]
        reordered = map (_2 .- sortBy (compare `on` (^. _2 & importModule & AST.moduleNameString))) parts

        parts = map (_2 .- reverse) $ reverse $ breakApart [] imports

        breakApart :: [(String, [(String, ImportDecl dom)])] -> [(String, ImportDecl dom)] -> [(String, [(String, ImportDecl dom)])]
        breakApart res [] = res
        breakApart res ((sep, e) : rest) | length (filter ('\n' ==) sep) > 1
          = breakApart ((sep, [("",e)]) : res) rest
        breakApart ((lastSep, lastRes) : res) (elem : rest)
          = breakApart ((lastSep, elem : lastRes) : res) rest
        breakApart [] ((sep, e) : rest)
          = breakApart [(sep, [("",e)])] rest

        imports = zipWithSeparators ls

-- | Modify an import to only import  names that are used.
narrowImports :: forall dom . OrganizeImportsDomain dom
              => Bool -> [String] -> [GHC.Name] -> [(GHC.Name, Bool)] -> [ClsInst] -> [FamInst] -> ImportDeclList dom -> LocalRefactor dom (ImportDeclList dom)
narrowImports noNarrowSubspecs exportedModules usedNames exportedNames prelInsts prelFamInsts imps
  = annListElems & traversal !~ narrowImport noNarrowSubspecs exportedModules usedNames exportedNames
      $ filterListIndexed (\i _ -> impsNeeded !! i) imps
  where impsNeeded = neededImports exportedModules (usedNames ++ map fst exportedNames) prelInsts prelFamInsts (imps ^. annListElems)

-- | Reduces the number of definitions used from an import
narrowImport :: OrganizeImportsDomain dom
             => Bool -> [String] -> [GHC.Name] -> [(GHC.Name, Bool)] -> ImportDecl dom -> LocalRefactor dom (ImportDecl dom)
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
       let -- to explicitely import pattern synonyms or type operators we need to enable an extension, and the user might not expect this
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

createImportSpec :: [(GHC.Name, Bool)] -> ImportSpec dom
createImportSpec elems = mkImportSpecList $ map createIESpec elems
  where createIESpec (n, False) = mkIESpec (mkUnqualName' (GHC.getName n)) Nothing
        createIESpec (n, True)  = mkIESpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

-- | Check each import if it is actually needed
neededImports :: OrganizeImportsDomain dom
              => [String] -> [GHC.Name] -> [ClsInst] -> [FamInst] -> [ImportDecl dom] -> [Bool]
neededImports exportedModules usedNames prelInsts prelFamInsts imps = neededImports' usedNames [] imps
  where neededImports' _ _ [] = []
        -- keep the import if any definition is needed from it
        neededImports' usedNames kept (imp : rest)
          | not (null actuallyImported)
               || (imp ^. importModule & moduleNameString) `elem` exportedModules
               || maybe False (`elem` exportedModules) (imp ^? importAs & annJust & importRename & moduleNameString)
            = True : neededImports' usedNames (imp : kept) rest
          where actuallyImported = semanticsImported imp `intersect` usedNames
        neededImports' usedNames kept (imp : rest)
            = needed : neededImports' usedNames (if needed then imp : kept else kept) rest
          where needed = any (`notElem` otherClsInstances) (map is_dfun $ semanticsOrphanInsts imp)
                           || any (`notElem` otherFamInstances) (map fi_axiom $ semanticsFamInsts imp)
                otherClsInstances = map is_dfun (concatMap semanticsOrphanInsts kept ++ prelInsts)
                otherFamInstances = map fi_axiom (concatMap semanticsFamInsts kept ++ prelFamInsts)

-- | Narrows the import specification (explicitely imported elements)
narrowImportSpecs :: forall dom . OrganizeImportsDomain dom
                  => Bool -> [GHC.Name] -> [(GHC.Name, Bool)] -> IESpecList dom -> LocalRefactor dom (IESpecList dom)
narrowImportSpecs noNarrowSubspecs usedNames exportedNames
  = (if noNarrowSubspecs then return else return . (annList .- narrowImportSubspecs neededNames exportedNames))
       >=> return . filterList isNeededSpec
  where neededNames = usedNames ++ map fst exportedNames

        isNeededSpec :: IESpec dom -> Bool
        isNeededSpec ie =
          (semanticsName (ie ^. ieName&simpleName)) `elem` map Just neededNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? ieSubspec&annJust&essList&annList) /= [])
            -- its a bit hard to decide if there is an element inside (for example bundled pattern synonyms)
            || (case ie ^? ieSubspec&annJust of Just SubAll -> True; _ -> False)

-- | Reduces the number of definitions imported from a sub-specifier.
narrowImportSubspecs :: OrganizeImportsDomain dom => [GHC.Name] -> [(GHC.Name, Bool)] -> IESpec dom -> IESpec dom
narrowImportSubspecs neededNames exportedNames ss | noNarrowingForThis = ss
  | otherwise
  = ieSubspec & annJust & essList .- filterList (\n -> (semanticsName (n ^. simpleName)) `elem` map Just neededNames) $ ss
  where noNarrowingForThis = case semanticsName (ss ^. ieName&simpleName) of
                               Just name -> lookup name exportedNames == Just True
                               _ -> False
