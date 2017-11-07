{-# LANGUAGE TupleSections
           , ConstraintKinds
           , TypeFamilies
           , FlexibleContexts
           , LambdaCase
           #-}
module Language.Haskell.Tools.Refactor.Builtin.GenerateExports
  (generateExports, DomGenerateExports, generateExportsRefactoring) where

import Control.Reference ((^?), (.=), (&))
import Language.Haskell.Tools.Refactor

import qualified GHC (NamedThing(..), Name)

import Control.Applicative ((<|>))
import Data.Maybe (Maybe(..), catMaybes)

generateExportsRefactoring :: (DomGenerateExports dom, HasModuleInfo dom) => RefactoringChoice dom
generateExportsRefactoring = ModuleRefactoring "GenerateExports" (localRefactoring generateExports)

-- | We need name information to generate exports
type DomGenerateExports dom = (Domain dom, HasNameInfo dom)

-- | Creates an export list that imports standalone top-level definitions with all of their contained definitions
generateExports :: DomGenerateExports dom => LocalRefactoring dom
generateExports mod = return (modHead & annJust & mhExports & annMaybe
                                .= Just (createExports (getTopLevels mod)) $ mod)

-- | Get all the top-level definitions with flags that mark if they can contain other top-level definitions
-- (classes and data declarations).
getTopLevels :: DomGenerateExports dom => Module dom -> [(GHC.Name, Bool)]
getTopLevels mod = catMaybes $ map (\d -> fmap (,exportContainOthers d)
                                               (foldl (<|>) Nothing $ map semanticsName $ d ^? elementName))
                                   (filter (\case TypeSigDecl{} -> False; _ -> True)
                                      $ mod ^? modDecl & annList)
  where exportContainOthers :: Decl dom -> Bool
        exportContainOthers (DataDecl {}) = True
        exportContainOthers (ClassDecl {}) = True
        exportContainOthers _ = False

-- | Create the export for a give name.
createExports :: [(GHC.Name, Bool)] -> ExportSpecs dom
createExports elems = mkExportSpecs $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIESpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIESpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)
