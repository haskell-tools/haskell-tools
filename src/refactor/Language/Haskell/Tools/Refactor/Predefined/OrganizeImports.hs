{-# LANGUAGE LambdaCase 
           , ScopedTypeVariables
           , FlexibleContexts
           , TypeFamilies
           , ConstraintKinds
           , TupleSections
           #-}
module Language.Haskell.Tools.Refactor.Predefined.OrganizeImports (organizeImports, OrganizeImportsDomain, projectOrganizeImports) where

import ConLike (ConLike(..))
import DataCon (FieldLbl(..), dataConTyCon)
import DynFlags (xopt)
import FamInstEnv (FamInst(..))
import GHC (TyThing(..), lookupName)
import qualified GHC
import Id
import IdInfo (RecSelParent(..))
import InstEnv (ClsInst(..))
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import Name (NamedThing(..))
import TyCon (tyConFieldLabels, tyConDataCons, isClassTyCon)

import Control.Applicative ((<$>), Alternative(..))
import Control.Monad
import Control.Monad.Trans (MonadTrans(..))
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
  = do ms <- lift $ GHC.getModSummary (GHC.moduleName $ semanticsModule mod)
       let th = xopt TemplateHaskell $ GHC.ms_hspp_opts ms
       if th 
         then -- don't change the imports for template haskell modules 
              -- (we don't know what definitions the generated code will use)
              return $ modImports .- sortImports $ mod 
         else modImports !~ narrowImports exportedModules usedNames prelInstances prelFamInsts . sortImports $ mod
  where prelInstances = semanticsPrelOrphanInsts mod
        prelFamInsts = semanticsPrelFamInsts mod
        usedNames = map getName $ catMaybes $ map semanticsName
                        -- obviously we don't want the names in the imports to be considered, but both from
                        -- the declarations (used), both from the module head (re-exported) will count as usage
                      $ (universeBi (mod ^. modHead) ++ universeBi (mod ^. modDecl) :: [QualifiedName dom])
        exportedModules = mod ^? modHead & annJust & mhExports & annJust 
                                   & espExports & annList & exportModuleName & moduleNameString
        
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
              => [String] -> [GHC.Name] -> [ClsInst] -> [FamInst] -> ImportDeclList dom -> LocalRefactor dom (ImportDeclList dom)
narrowImports exportedModules usedNames prelInsts prelFamInsts imps 
  = annListElems & traversal !~ narrowImport exportedModules usedNames 
      $ filterListIndexed (\i _ -> neededImps !! i) imps
  where neededImps = neededImports exportedModules usedNames prelInsts prelFamInsts (imps ^. annListElems)

-- | Reduces the number of definitions used from an import
narrowImport :: OrganizeImportsDomain dom
             => [String] -> [GHC.Name] -> ImportDecl dom -> LocalRefactor dom (ImportDecl dom)
narrowImport exportedModules usedNames imp
  | (imp ^. importModule & moduleNameString) `elem` exportedModules
      || maybe False (`elem` exportedModules) (imp ^? importAs & annJust & importRename & moduleNameString)
  = return imp -- dont change an import if it is exported as-is (module export)
  | importIsExact imp
  = importSpec&annJust&importSpecList !~ narrowImportSpecs usedNames $ imp
  | otherwise
  = do namedThings <- mapM lookupName actuallyImported
       let -- to explicitely import pattern synonyms we need to enable an extension, and the user might not expect this
           hasPatSyn = any (\case Just (AConLike (PatSynCon _)) -> True; _ -> False) namedThings
           groups = groupThings (semanticsImported imp) (catMaybes namedThings)
       return $ if not hasPatSyn && length groups < 4 
         then importSpec .- replaceWithJust (createImportSpec groups) $ imp
         else imp 
  where actuallyImported = semanticsImported imp `intersect` usedNames

groupThings :: [GHC.Name] -> [TyThing] -> [(GHC.Name, Bool)]
groupThings importable = nub . sort . map createImportFromTyThing
  where createImportFromTyThing :: TyThing -> (GHC.Name, Bool)
        createImportFromTyThing tt | Just td <- getTopDef tt
          = if (td `elem` importable) then (td, True) 
                                      else (getName tt, False)
          | otherwise = (getName tt, False)

getTopDef :: TyThing -> Maybe GHC.Name
getTopDef (AnId id) | isRecordSelector id
  = Just $ case recordSelectorTyCon id of RecSelData tc -> getName tc
                                          RecSelPatSyn ps -> getName ps
getTopDef (AnId id) = fmap (getName . dataConTyCon) (isDataConWorkId_maybe id <|> isDataConId_maybe id) 
                        <|> fmap getName (isClassOpId_maybe id)
getTopDef (AConLike (RealDataCon dc)) = Just (getName $ dataConTyCon dc)
getTopDef (AConLike (PatSynCon _)) = error "getTopDef: should not be called with pattern synonyms"
getTopDef tc@(ATyCon _) = Just (getName tc)

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
                  => [GHC.Name] -> IESpecList dom -> LocalRefactor dom (IESpecList dom)
narrowImportSpecs usedNames 
  = (annList !~ narrowSpecSubspec usedNames) 
       >=> return . filterList isNeededSpec
  where narrowSpecSubspec :: [GHC.Name] -> IESpec dom -> LocalRefactor dom (IESpec dom)
        narrowSpecSubspec usedNames spec 
          = do let Just specName = semanticsName =<< (spec ^? ieName&simpleName)
               Just tt <- GHC.lookupName (getName specName)
               let subspecsInScope = case tt of ATyCon tc | not (isClassTyCon tc) 
                                                  -> (map getName (tyConDataCons tc) ++ map flSelector (tyConFieldLabels tc)) `intersect` usedNames
                                                _ -> usedNames
               ieSubspec&annJust !- narrowImportSubspecs subspecsInScope $ spec
  
        isNeededSpec :: IESpec dom -> Bool
        isNeededSpec ie = 
          -- if the name is used, it is needed
          fmap getName (semanticsName =<< (ie ^? ieName&simpleName)) `elem` map Just usedNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? ieSubspec&annJust&essList&annList) /= [])
            || (case ie ^? ieSubspec&annJust of Just SubAll -> True; _ -> False)     

-- | Reduces the number of definitions imported from a sub-specifier.
narrowImportSubspecs :: OrganizeImportsDomain dom => [GHC.Name] -> SubSpec dom -> SubSpec dom
narrowImportSubspecs [] SubAll = mkSubList []
narrowImportSubspecs _ ss@SubAll = ss
narrowImportSubspecs usedNames ss@(SubList {}) 
  = essList .- filterList (\n -> fmap getName (semanticsName =<< (n ^? simpleName)) `elem` map Just usedNames) $ ss


