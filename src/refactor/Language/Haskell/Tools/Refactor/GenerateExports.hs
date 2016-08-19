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
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

type DomGenerateExports dom = (Domain dom, HasNameInfo (SemanticInfo' dom SameInfoNameCls))

-- | Creates an export list that imports standalone top-level definitions with all of their contained definitions
generateExports :: DomGenerateExports dom => LocalRefactoring dom
generateExports mod = return (element & modHead & annJust & element & mhExports & annMaybe .= Just (createExports (getTopLevels mod)) $ mod)

-- | Get all the top-level definitions with flags that mark if they can contain other top-level definitions 
-- (classes and data declarations).
getTopLevels :: DomGenerateExports dom => Ann Module dom SrcTemplateStage -> [(GHC.Name, Bool)]
getTopLevels mod = catMaybes $ map (\d -> fmap (,exportContainOthers d) (getTopLevelDeclName d)) (mod ^? element & modDecl & annList & element)
  where exportContainOthers :: Decl dom SrcTemplateStage -> Bool
        exportContainOthers (DataDecl {}) = True
        exportContainOthers (ClassDecl {}) = True
        exportContainOthers _ = False

-- | Get all the standalone top level definitions (their GHC unique names) in a module. 
-- You could also do getting all the names with a biplate reference and select the top-level ones, but this is more efficient.
getTopLevelDeclName :: DomGenerateExports dom => Decl dom SrcTemplateStage -> Maybe GHC.Name
getTopLevelDeclName (d @ TypeDecl {}) = semanticsName =<< listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ TypeFamilyDecl {}) = semanticsName =<< listToMaybe (d ^? declTypeFamily & element & tfHead & dhNames)
getTopLevelDeclName (d @ ClosedTypeFamilyDecl {}) = semanticsName =<< listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ DataDecl {}) = semanticsName =<< listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ GDataDecl {}) = semanticsName =<< listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ ClassDecl {}) = semanticsName =<< listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ PatternSynonymDecl {}) 
  = semanticsName =<< listToMaybe (d ^? declPatSyn & element & patLhs & element & (patName & element & simpleName &+& patSynOp & element & operatorName) & semantics)
getTopLevelDeclName (d @ ValueBinding {}) = semanticsName =<< listToMaybe (d ^? declValBind & bindingName)
getTopLevelDeclName (d @ ForeignImport {}) = semanticsName =<< listToMaybe (d ^? declName & element & simpleName & semantics)
getTopLevelDeclName _ = Nothing

-- | Create the export for a give name.
createExports :: DomGenerateExports dom => [(GHC.Name, Bool)] -> Ann ExportSpecList dom SrcTemplateStage
createExports elems = mkExportSpecList $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIeSpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIeSpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

