{-# LANGUAGE TupleSections
           , ConstraintKinds
           , TypeFamilies
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Refactor.GenerateExports where

import Control.Reference hiding (element)

import qualified GHC

import Data.Maybe

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Refactor.RefactorBase

type DomGenerateExports dom = (Domain dom, HasNameInfo dom)

-- | Creates an export list that imports standalone top-level definitions with all of their contained definitions
generateExports :: DomGenerateExports dom => LocalRefactoring dom
generateExports mod = return (element & modHead & annJust & element & mhExports & annMaybe 
                                .= Just (createExports (getTopLevels mod)) $ mod)

-- | Get all the top-level definitions with flags that mark if they can contain other top-level definitions 
-- (classes and data declarations).
getTopLevels :: DomGenerateExports dom => Ann Module dom SrcTemplateStage -> [(GHC.Name, Bool)]
getTopLevels mod = catMaybes $ map (\d -> fmap (,exportContainOthers d) (listToMaybe $ elementName d)) (mod ^? element & modDecl & annList)
  where exportContainOthers :: Ann Decl dom SrcTemplateStage -> Bool
        exportContainOthers (DataDecl {}) = True
        exportContainOthers (ClassDecl {}) = True
        exportContainOthers _ = False

-- | Create the export for a give name.
createExports :: DomGenerateExports dom => [(GHC.Name, Bool)] -> Ann ExportSpecList dom SrcTemplateStage
createExports elems = mkExportSpecList $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIeSpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIeSpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

