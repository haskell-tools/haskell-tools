{-# LANGUAGE TupleSections #-}
module Language.Haskell.Tools.Refactor.GenerateExports where

import Control.Reference hiding (element)

import qualified GHC

import Data.Maybe

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

-- | Creates an export list that imports standalone top-level definitions with all of their contained definitions
generateExports :: GHC.NamedThing n => Ann Module (STWithNames n) -> RefactoredModule n
generateExports mod = return (element & modHead & annJust & element & mhExports & annMaybe .= Just (createExports (getTopLevels mod)) $ mod)

-- | Get all the top-level definitions with flags that mark if they can contain other top-level definitions 
-- (classes and data declarations).
getTopLevels :: Ann Module (STWithNames n) -> [(n, Bool)]
getTopLevels mod = catMaybes $ map (\d -> fmap (,exportContainOthers d) (getTopLevelDeclName d)) (mod ^? element & modDecl & annList & element)
  where exportContainOthers :: Decl (STWithNames n) -> Bool
        exportContainOthers (DataDecl {}) = True
        exportContainOthers (ClassDecl {}) = True
        exportContainOthers _ = False

-- | Get all the standalone top level definitions (their GHC unique names) in a module. 
-- You could also do getting all the names with a biplate reference and select the top-level ones, but this is more efficient.
getTopLevelDeclName :: Decl (NodeInfo (SemanticInfo n) src) -> Maybe n
getTopLevelDeclName (d @ TypeDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ TypeFamilyDecl {}) = listToMaybe (d ^? declTypeFamily & element & tfHead & dhNames)
getTopLevelDeclName (d @ ClosedTypeFamilyDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ DataDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ GDataDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ ClassDecl {}) = listToMaybe (d ^? declHead & dhNames)
getTopLevelDeclName (d @ PatternSynonymDecl {}) 
  = listToMaybe (d ^? declPatSyn & element & patLhs & element & (patName & element & simpleName &+& patSynOp & element & operatorName) & semantics & nameInfo)
getTopLevelDeclName (d @ ValueBinding {}) = listToMaybe (d ^? declValBind & bindingName)
getTopLevelDeclName (d @ ForeignImport {}) = listToMaybe (d ^? declName & element & simpleName & semantics & nameInfo)
getTopLevelDeclName _ = Nothing

-- | Create the export for a give name.
createExports :: GHC.NamedThing n => [(n, Bool)] -> Ann ExportSpecList (STWithNames n)
createExports elems = mkExportSpecList $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIeSpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIeSpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

