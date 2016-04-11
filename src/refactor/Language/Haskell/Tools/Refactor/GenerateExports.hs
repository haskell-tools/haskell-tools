module Language.Haskell.Tools.Refactor.GenerateExports where

import Control.Reference hiding (element)

import qualified GHC

import Data.Maybe

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen

type STWithNames n = NodeInfo (SemanticInfo n) SourceTemplate

generateExports :: GHC.NamedThing n => Ann Module (STWithNames n) -> GHC.Ghc (Ann Module (STWithNames n))
generateExports mod = return (element & modHead & annJust & element & mhExports & annMaybe .= Just (createExports (getTopLevels mod)) $ mod)

getTopLevels :: Ann Module (STWithNames n) -> [(n, Bool)]
getTopLevels mod = catMaybes $ map getDeclExport (mod ^? element & modDecl & annList & element)
  where getDeclExport :: Decl (STWithNames n) -> Maybe (n, Bool)
        getDeclExport (d @ TypeDecl {}) = Just (head (d ^? declHead & dhNames), False)
        getDeclExport (d @ TypeFamilyDecl {}) = Just (head (d ^? declTypeFamily & element & tfHead & dhNames), True)
        getDeclExport (d @ ClosedTypeFamilyDecl {}) = Just (head (d ^? declHead & dhNames), True)
        getDeclExport (d @ DataDecl {}) = Just (head (d ^? declHead & dhNames), True)
        getDeclExport (d @ GDataDecl {}) = Just (head (d ^? declHead & dhNames), True)
        getDeclExport (d @ ClassDecl {}) = Just (head (d ^? declHead & dhNames), True)
        getDeclExport (d @ PatternSynonymDecl {}) = Just (fromJust (d ^? declPatSyn & element & patName & semantics & nameInfo), False)
        getDeclExport (d @ ValueBinding {}) = Just (head (d ^? declValBind & bindingName), False)
        getDeclExport _ = Nothing
        dhNames = declHeadNames & semantics & nameInfo

createExports :: GHC.NamedThing n => [(n, Bool)] -> Ann ExportSpecList (STWithNames n)
createExports elems = mkExportSpecList $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIeSpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIeSpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

