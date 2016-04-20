{-# LANGUAGE TupleSections #-}
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
getTopLevels mod = catMaybes $ map (\d -> fmap (,exportContainOthers d) (getTopLevelDeclName d)) (mod ^? element & modDecl & annList & element)
  where exportContainOthers :: Decl (STWithNames n) -> Bool
        exportContainOthers (TypeDecl {}) = False
        exportContainOthers (PatternSynonymDecl {}) = False
        exportContainOthers (ValueBinding {}) = False
        exportContainOthers _ = True

createExports :: GHC.NamedThing n => [(n, Bool)] -> Ann ExportSpecList (STWithNames n)
createExports elems = mkExportSpecList $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIeSpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIeSpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)

