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
      $ Module (mkAnnList (listSepAfter "\n" "\n") filePrags) (mkAnnMaybe opt head)
               (mkAnnList (indentedListBefore "\n") imps) (mkAnnList (indentedListBefore "\n") decls)
               
mkModuleHead :: Ann ModuleName dom SrcTemplateStage -> Maybe (Ann ExportSpecList dom SrcTemplateStage) 
                  -> Maybe (Ann ModulePragma dom SrcTemplateStage) -> Ann ModuleHead dom SrcTemplateStage
mkModuleHead n es pr = mkAnn ("module " <> child <> child <> child <> " where") $ ModuleHead n (mkAnnMaybe opt es) (mkAnnMaybe (optBefore "\n") pr)

mkExportSpecList :: [Ann ExportSpec dom SrcTemplateStage] -> Ann ExportSpecList dom SrcTemplateStage
mkExportSpecList = mkAnn ("(" <> child <> ")") . ExportSpecList . mkAnnList (listSep ", ")

mkModuleExport :: Ann ModuleName dom SrcTemplateStage -> Ann ExportSpec dom SrcTemplateStage
mkModuleExport = mkAnn ("module " <> child) . ModuleExport

mkExportSpec :: Ann IESpec dom SrcTemplateStage -> Ann ExportSpec dom SrcTemplateStage
mkExportSpec = mkAnn child . DeclExport

mkIeSpec :: Ann Name dom SrcTemplateStage -> Maybe (Ann SubSpec dom SrcTemplateStage) -> Ann IESpec dom SrcTemplateStage
mkIeSpec name ss = mkAnn (child <> child) (IESpec name (mkAnnMaybe opt ss))
        
mkSubList :: [Ann Name dom SrcTemplateStage] -> Ann SubSpec dom SrcTemplateStage
mkSubList = mkAnn ("(" <> child <> ")") . SubSpecList . mkAnnList (listSep ", ")

mkSubAll :: Ann SubSpec dom SrcTemplateStage
mkSubAll = mkAnn "(..)" SubSpecAll

mkImportDecl :: Bool -> Bool -> Bool -> Maybe String -> Ann ModuleName dom SrcTemplateStage 
                     -> Maybe String -> Maybe (Ann ImportSpec dom SrcTemplateStage) 
                     -> Ann ImportDecl dom SrcTemplateStage       
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      ImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " ImportSource) else noth)
                 (if qualified then justVal (mkAnn "qualified " ImportQualified) else noth)
                 (if safe then justVal (mkAnn "safe " ImportSafe) else noth)
                 (case pkg of Just str -> justVal (mkAnn (fromString $ str ++ " ") (StringNode str)); _ -> noth)
                 name (mkAnnMaybe opt (fmap (mkAnn (" as " <> child) . ImportRenaming . mkModuleName) rename)) (mkAnnMaybe opt spec)

mkImportSpecList :: [Ann IESpec dom SrcTemplateStage] -> Ann ImportSpec dom SrcTemplateStage
mkImportSpecList = mkAnn ("(" <> child <> ")") . ImportSpecList . mkAnnList (listSep ", ")

mkImportHidingList :: [Ann IESpec dom SrcTemplateStage] -> Ann ImportSpec dom SrcTemplateStage
mkImportHidingList = mkAnn (" hiding (" <> child <> ")") . ImportSpecHiding . mkAnnList (listSep ", ")