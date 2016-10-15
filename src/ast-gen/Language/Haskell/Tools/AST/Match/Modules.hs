-- | Pattern matching on Module-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Modules where

import Language.Haskell.Tools.AST

pattern Module :: AnnList FilePragma dom SrcTemplateStage -> AnnMaybe ModuleHead dom SrcTemplateStage
              -> AnnList ImportDecl dom SrcTemplateStage -> AnnList Decl dom SrcTemplateStage -> Ann Module dom SrcTemplateStage
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

pattern ModuleHead :: Ann ModuleName dom SrcTemplateStage -> AnnMaybe ExportSpecList dom SrcTemplateStage 
                  -> AnnMaybe ModulePragma dom SrcTemplateStage -> Ann ModuleHead dom SrcTemplateStage
pattern ModuleHead n es pr <- Ann _ (UModuleHead n es pr)

pattern ExportSpecList :: AnnList ExportSpec dom SrcTemplateStage -> Ann ExportSpecList dom SrcTemplateStage
pattern ExportSpecList specs <- Ann _ (UExportSpecList specs)

pattern ModuleExport :: Ann ModuleName dom SrcTemplateStage -> Ann ExportSpec dom SrcTemplateStage
pattern ModuleExport name <- Ann _ (UModuleExport name)

pattern ExportSpec :: Ann IESpec dom SrcTemplateStage -> Ann ExportSpec dom SrcTemplateStage
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

pattern IESpec :: Ann Name dom SrcTemplateStage -> AnnMaybe SubSpec dom SrcTemplateStage -> Ann IESpec dom SrcTemplateStage
pattern IESpec name ss <- Ann _ (UIESpec name ss)

pattern SubList :: AnnList Name dom SrcTemplateStage -> Ann SubSpec dom SrcTemplateStage
pattern SubList names <- Ann _ (USubSpecList names)

pattern SubAll :: Ann SubSpec dom SrcTemplateStage
pattern SubAll <- Ann _ USubSpecAll

pattern ImportDecl :: AnnMaybe ImportSource dom SrcTemplateStage -> AnnMaybe ImportQualified dom SrcTemplateStage 
                        -> AnnMaybe ImportSafe dom SrcTemplateStage -> AnnMaybe StringNode dom SrcTemplateStage
                        -> Ann ModuleName dom SrcTemplateStage -> AnnMaybe ImportRenaming dom SrcTemplateStage
                        -> AnnMaybe ImportSpec dom SrcTemplateStage -> Ann ImportDecl dom SrcTemplateStage       
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: Ann ModuleName dom SrcTemplateStage -> Ann ImportRenaming dom SrcTemplateStage
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: AnnList IESpec dom SrcTemplateStage -> Ann ImportSpec dom SrcTemplateStage
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: AnnList IESpec dom SrcTemplateStage -> Ann ImportSpec dom SrcTemplateStage
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)
