-- | Generation of Module-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkModule@ creates the annotated version of the @Module@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
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

mkModule :: [Ann FilePragma dom SrcTemplateStage] -> Maybe (Ann ModuleHead dom SrcTemplateStage)
              -> [Ann ImportDecl dom SrcTemplateStage] -> [Ann Decl dom SrcTemplateStage] -> Ann Module dom SrcTemplateStage
mkModule filePrags head imps decls 
  = mkAnn (child <> child <> child <> child) 
      $ UModule (mkAnnList (listSepAfter "\n" "\n") filePrags) (mkAnnMaybe opt head)
                (mkAnnList (indentedListBefore "\n") imps) (mkAnnList (indentedListBefore "\n") decls)
               
mkModuleHead :: Ann ModuleName dom SrcTemplateStage -> Maybe (Ann ExportSpecList dom SrcTemplateStage) 
                  -> Maybe (Ann ModulePragma dom SrcTemplateStage) -> Ann ModuleHead dom SrcTemplateStage
mkModuleHead n es pr = mkAnn ("module " <> child <> child <> child <> " where") $ UModuleHead n (mkAnnMaybe opt es) (mkAnnMaybe (optBefore "\n") pr)

mkExportSpecList :: [Ann ExportSpec dom SrcTemplateStage] -> Ann ExportSpecList dom SrcTemplateStage
mkExportSpecList = mkAnn ("(" <> child <> ")") . UExportSpecList . mkAnnList (listSep ", ")

mkModuleExport :: Ann ModuleName dom SrcTemplateStage -> Ann ExportSpec dom SrcTemplateStage
mkModuleExport = mkAnn ("module " <> child) . UModuleExport

mkExportSpec :: Ann IESpec dom SrcTemplateStage -> Ann ExportSpec dom SrcTemplateStage
mkExportSpec = mkAnn child . UDeclExport

mkIeSpec :: Ann Name dom SrcTemplateStage -> Maybe (Ann SubSpec dom SrcTemplateStage) -> Ann IESpec dom SrcTemplateStage
mkIeSpec name ss = mkAnn (child <> child) (UIESpec name (mkAnnMaybe opt ss))
        
mkSubList :: [Ann Name dom SrcTemplateStage] -> Ann SubSpec dom SrcTemplateStage
mkSubList = mkAnn ("(" <> child <> ")") . USubSpecList . mkAnnList (listSep ", ")

mkSubAll :: Ann SubSpec dom SrcTemplateStage
mkSubAll = mkAnn "(..)" USubSpecAll

mkImportDecl :: Bool -> Bool -> Bool -> Maybe String -> Ann ModuleName dom SrcTemplateStage 
                     -> Maybe String -> Maybe (Ann ImportSpec dom SrcTemplateStage) 
                     -> Ann ImportDecl dom SrcTemplateStage       
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      UImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " UImportSource) else noth)
                 (if qualified then justVal (mkAnn "qualified " UImportQualified) else noth)
                 (if safe then justVal (mkAnn "safe " UImportSafe) else noth)
                 (case pkg of Just str -> justVal (mkStringNode str); _ -> noth)
                 name (mkAnnMaybe opt (fmap (mkAnn (" as " <> child) . UImportRenaming . mkModuleName) rename)) (mkAnnMaybe opt spec)

mkImportSpecList :: [Ann IESpec dom SrcTemplateStage] -> Ann ImportSpec dom SrcTemplateStage
mkImportSpecList = mkAnn ("(" <> child <> ")") . UImportSpecList . mkAnnList (listSep ", ")

mkImportHidingList :: [Ann IESpec dom SrcTemplateStage] -> Ann ImportSpec dom SrcTemplateStage
mkImportHidingList = mkAnn (" hiding (" <> child <> ")") . UImportSpecHiding . mkAnnList (listSep ", ")