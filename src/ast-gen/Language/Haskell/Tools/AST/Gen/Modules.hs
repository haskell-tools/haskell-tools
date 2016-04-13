-- | Generation of Module-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Tools.AST.Gen.Modules where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkModule :: TemplateAnnot a => [Ann ModulePragma a] -> Maybe (Ann ModuleHead a) 
                            -> [Ann ImportDecl a] -> [Ann Decl a] -> Ann Module a
mkModule pragmas head imps decls 
  = mkAnn (child <> "\n" <> child <> "\n" <> child <> "\n" <> child) 
      $ Module (mkAnnList (listSep "\n") pragmas) (mkAnnMaybe opt head)
               (mkAnnList indentedList imps) (mkAnnList indentedList decls)
               
mkModuleHead :: TemplateAnnot a => Ann Name a -> Maybe (Ann ExportSpecList a) -> Ann ModuleHead a
mkModuleHead n es = mkAnn (child <> child) $ ModuleHead n (mkAnnMaybe (optBefore " ") es)

mkExportSpecList :: TemplateAnnot a => [Ann ExportSpec a] -> Ann ExportSpecList a
mkExportSpecList = mkAnn ("(" <> child <> ")") . ExportSpecList . mkAnnList (listSep ", ")

mkModuleExport :: TemplateAnnot a => Ann Name a -> Ann ExportSpec a
mkModuleExport = mkAnn ("module " <> child) . ModuleExport

mkExportSpec :: TemplateAnnot a => Ann IESpec a -> Ann ExportSpec a
mkExportSpec = mkAnn child . DeclExport

mkImportSpecList :: TemplateAnnot a => [Ann IESpec a] -> Ann ImportSpec a
mkImportSpecList = mkAnn ("(" <> child <> ")") . ImportSpecList . mkAnnList list

mkIeSpec :: TemplateAnnot a => Ann Name a -> Maybe (Ann SubSpec a) -> Ann IESpec a
mkIeSpec name ss = mkAnn (child <> child) (IESpec name (mkAnnMaybe opt ss))
        
mkSubList :: TemplateAnnot a => [Ann Name a] -> Ann SubSpec a
mkSubList = mkAnn ("(" <> child <> ")") . SubSpecList . mkAnnList list

mkSubAll :: TemplateAnnot a => Ann SubSpec a
mkSubAll = mkAnn "(..)" SubSpecAll

-- TODO: mk pragmas

mkImportDecl :: TemplateAnnot a => Bool -> Bool -> Bool -> Maybe String -> Ann Name a -> Maybe (Ann ImportRenaming a) -> Maybe (Ann ImportSpec a) -> Ann ImportDecl a       
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      ImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " ImportSource) else noth)
                 (if qualified then justVal (mkAnn "qualified " ImportQualified) else noth)
                 (if safe then justVal (mkAnn "safe " ImportSafe) else noth)
                 (case pkg of Just str -> justVal (mkAnn (fromString $ str ++ " ") (StringNode str)); _ -> noth)
                 name (mkAnnMaybe opt rename) (mkAnnMaybe opt spec)