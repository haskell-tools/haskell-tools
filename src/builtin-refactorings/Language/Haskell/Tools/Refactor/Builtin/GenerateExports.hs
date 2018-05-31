{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Tools.Refactor.Builtin.GenerateExports
  (generateExports, generateExportsRefactoring) where

import Control.Reference ((^?), (.=), (&))
import Language.Haskell.Tools.Refactor

import qualified GHC (NamedThing(..), Name)

import Control.Applicative ((<|>))
import Data.Maybe (Maybe(..), catMaybes)

generateExportsRefactoring :: RefactoringChoice
generateExportsRefactoring = ModuleRefactoring "GenerateExports" (localRefactoring generateExports)

-- | Creates an export list that imports standalone top-level definitions with all of their contained definitions
generateExports :: LocalRefactoring
generateExports mod = return (modHead & annJust & mhExports & annMaybe
                                .= Just (createExports (getTopLevels mod)) $ mod)

-- | Get all the top-level definitions with flags that mark if they can contain other top-level definitions
-- (classes and data declarations).
getTopLevels :: Module -> [(GHC.Name, Bool)]
getTopLevels mod = catMaybes $ map (\d -> fmap (,exportContainOthers d)
                                               (foldl (<|>) Nothing $ map semanticsName $ d ^? elementName))
                                   (filter (\case TypeSigDecl{} -> False; _ -> True)
                                      $ mod ^? modDecl & annList)
  where exportContainOthers :: Decl -> Bool
        exportContainOthers (DataDecl {}) = True
        exportContainOthers (ClassDecl {}) = True
        exportContainOthers _ = False

-- | Create the export for a give name.
createExports :: [(GHC.Name, Bool)] -> ExportSpecs
createExports elems = mkExportSpecs $ map (mkExportSpec . createExport) elems
  where createExport (n, False) = mkIESpec (mkUnqualName' (GHC.getName n)) Nothing
        createExport (n, True)  = mkIESpec (mkUnqualName' (GHC.getName n)) (Just mkSubAll)
