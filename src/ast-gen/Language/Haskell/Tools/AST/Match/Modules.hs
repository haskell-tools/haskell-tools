-- | UPattern matching on UModule-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Modules where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
pattern Module :: FilePragmaList dom -> MaybeModuleHead dom
              -> ImportDeclList dom -> DeclList dom -> Module dom
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

-- | Module declaration with name and (optional) exports
pattern ModuleHead :: ModuleName dom -> MaybeExportSpecs dom 
                  -> MaybeModulePragma dom -> ModuleHead dom
pattern ModuleHead n es pr <- Ann _ (UModuleHead n es pr)

-- | A list of export specifications surrounded by parentheses
pattern ExportSpecs :: ExportSpecList dom -> ExportSpecs dom
pattern ExportSpecs specs <- Ann _ (UExportSpecs specs)

-- | Export a name and related names
pattern ExportSpec :: IESpec dom -> ExportSpec dom
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

-- | The export of an imported module (@ module A @)
pattern ModuleExport :: ModuleName dom -> ExportSpec dom
pattern ModuleExport name <- Ann _ (UModuleExport name)

-- | Marks a name to be imported or exported with related names (subspecifier)
pattern IESpec :: Name dom -> MaybeSubSpec dom -> IESpec dom
pattern IESpec name ss <- Ann _ (UIESpec name ss)

-- | @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
pattern SubAll :: SubSpec dom
pattern SubAll <- Ann _ USubSpecAll

-- | @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
pattern SubList :: NameList dom -> SubSpec dom
pattern SubList names <- Ann _ (USubSpecList names)


pattern ImportDecl :: MaybeImportSource dom -> MaybeImportQualified dom 
                        -> MaybeImportSafe dom -> MaybeStringNode dom
                        -> ModuleName dom -> MaybeImportRenaming dom
                        -> MaybeImportSpec dom -> ImportDecl dom       
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: ModuleName dom -> ImportRenaming dom
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: IESpecList dom -> ImportSpec dom
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: IESpecList dom -> ImportSpec dom
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)

pattern ModuleName :: String -> ModuleName dom
pattern ModuleName s <- Ann _ (UModuleName s)

-- * Pragmas

-- | @LANGUAGE@ pragma, listing the enabled language extensions in that file
pattern LanguagePragma :: LanguageExtensionList dom -> FilePragma dom
pattern LanguagePragma exts <- Ann _ (ULanguagePragma exts)

-- | @OPTIONS@ pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
pattern OptionsPragma :: String -> FilePragma dom
pattern OptionsPragma opt <- Ann _ (UOptionsPragma (Ann _ (UStringNode opt)))

-- | The name of the enabled language extension, for example (@ LambdaCase @)
pattern LanguageExtension :: String -> LanguageExtension dom
pattern LanguageExtension ext <- Ann _ (ULanguageExtension ext)

-- | A warning pragma attached to the module
pattern ModuleWarningPragma :: StringNodeList dom -> ModulePragma dom
pattern ModuleWarningPragma msgs <- Ann _ (UModuleWarningPragma msgs)

-- | A deprecated pragma attached to the module
pattern ModuleDeprecatedPragma :: StringNodeList dom -> ModulePragma dom
pattern ModuleDeprecatedPragma msgs <- Ann _ (UModuleDeprecatedPragma msgs)