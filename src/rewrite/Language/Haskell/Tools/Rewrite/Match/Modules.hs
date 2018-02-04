-- | UPattern matching on UModule-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}

module Language.Haskell.Tools.Rewrite.Match.Modules where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
pattern Module :: FilePragmaList -> MaybeModuleHead
              -> ImportDeclList -> DeclList -> Module
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

-- | Module declaration with name and (optional) exports
pattern ModuleHead :: ModuleName -> MaybeModulePragma -> MaybeExportSpecs -> ModuleHead
pattern ModuleHead n pr es <- Ann _ (UModuleHead n pr es)

-- | A list of export specifications surrounded by parentheses
pattern ExportSpecs :: ExportSpecList -> ExportSpecs
pattern ExportSpecs specs <- Ann _ (UExportSpecs specs)

-- | Export a name and related names
pattern ExportSpec :: IESpec -> ExportSpec
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

-- | The export of an imported module (@ module A @)
pattern ModuleExport :: ModuleName -> ExportSpec
pattern ModuleExport name <- Ann _ (UModuleExport name)

-- | Marks a name to be imported or exported with related names (subspecifier)
pattern IESpec :: MaybeImportModifier -> Name -> MaybeSubSpec -> IESpec
pattern IESpec modifier name ss <- Ann _ (UIESpec modifier name ss)

-- | @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
pattern SubAll :: SubSpec
pattern SubAll <- Ann _ USubSpecAll

-- | @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
pattern SubList :: NameList -> SubSpec
pattern SubList names <- Ann _ (USubSpecList names)


pattern ImportDecl :: MaybeImportSource -> MaybeImportQualified
                        -> MaybeImportSafe -> MaybeStringNode
                        -> ModuleName -> MaybeImportRenaming
                        -> MaybeImportSpec -> ImportDecl
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: ModuleName -> ImportRenaming
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: IESpecList -> ImportSpec
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: IESpecList -> ImportSpec
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)

pattern ModuleName :: String -> ModuleName
pattern ModuleName s <- Ann _ (UModuleName s)

-- * Pragmas

-- | @LANGUAGE@ pragma, listing the enabled language extensions in that file
pattern LanguagePragma :: LanguageExtensionList -> FilePragma
pattern LanguagePragma exts <- Ann _ (ULanguagePragma exts)

-- | @OPTIONS@ pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
pattern OptionsPragma :: String -> FilePragma
pattern OptionsPragma opt <- Ann _ (UOptionsPragma (Ann _ (UStringNode opt)))

-- | The name of the enabled language extension, for example (@ LambdaCase @)
pattern LanguageExtension :: String -> LanguageExtension
pattern LanguageExtension ext <- Ann _ (ULanguageExtension ext)

-- | A warning pragma attached to the module
pattern ModuleWarningPragma :: StringNodeList -> ModulePragma
pattern ModuleWarningPragma msgs <- Ann _ (UModuleWarningPragma msgs)

-- | A deprecated pragma attached to the module
pattern ModuleDeprecatedPragma :: StringNodeList -> ModulePragma
pattern ModuleDeprecatedPragma msgs <- Ann _ (UModuleDeprecatedPragma msgs)
