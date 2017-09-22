-- | Representation of Haskell modules, imports and exports. Also contains file-level pragmas.
module Language.Haskell.Tools.AST.Representation.Modules where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG, AnnMaybeG)
import Language.Haskell.Tools.AST.Representation.Decls (UDecl)
import Language.Haskell.Tools.AST.Representation.Names (UStringNode, UName)

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
data UModule dom stage
  = UModule { _filePragmas :: AnnListG UFilePragma dom stage
            , _modHead :: AnnMaybeG UModuleHead dom stage
            , _modImports :: AnnListG UImportDecl dom stage
            , _modDecl :: AnnListG UDecl dom stage
            }

-- | Module declaration with name and (optional) exports
data UModuleHead dom stage
  = UModuleHead { _mhName :: Ann UModuleName dom stage
                , _mhPragma :: AnnMaybeG UModulePragma dom stage
                , _mhExports :: AnnMaybeG UExportSpecs dom stage
                }

-- | A list of export specifications surrounded by parentheses
data UExportSpecs dom stage
  = UExportSpecs { _espExports :: AnnListG UExportSpec dom stage }

-- | Export specifier
data UExportSpec dom stage
  = UDeclExport { _exportDecl :: Ann UIESpec dom stage
                } -- ^ Export a name and related names
  | UModuleExport { _exportModuleName :: Ann UModuleName dom stage
                  } -- ^ The export of an imported module (@ module A @)

-- | Marks a name to be imported or exported with related names (subspecifier)
data UIESpec dom stage
  = UIESpec { _ieModifier :: AnnMaybeG UImportModifier dom stage
            , _ieName :: Ann UName dom stage
            , _ieSubspec :: AnnMaybeG USubSpec dom stage
            }

-- | Specifies the imported element
data UImportModifier dom stage
  = UImportPattern -- ^ @pattern@: modifier for importing pattern synonyms
  | UImportType -- ^ @type@: modifier for importing types

-- | Marks how related names will be imported or exported with a given name
data USubSpec dom stage
  = USubSpecAll -- ^ @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | USubSpecList { _essList :: AnnListG UName dom stage
                 } -- ^ @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.

-- | Pragmas that must be used before defining the module
data UFilePragma dom stage
  = ULanguagePragma { _lpPragmas :: AnnListG ULanguageExtension dom stage
                    } -- ^ @LANGUAGE@ pragma, listing the enabled language extensions in that file
  | UOptionsPragma {  _opStr :: Ann UStringNode dom stage
                   } -- ^ @OPTIONS@ pragma, possibly qualified with a tool, e.g. OPTIONS_GHC

-- | Pragmas that must be used after the module head
data UModulePragma dom stage
  = UModuleWarningPragma { _modWarningStr :: AnnListG UStringNode dom stage
                         }  -- ^ A warning pragma attached to the module
  | UModuleDeprecatedPragma {  _modDeprecatedPragma :: AnnListG UStringNode dom stage
                            } -- ^ A deprecated pragma attached to the module

-- | The name of the enabled language extension, for example (@ LambdaCase @)
data ULanguageExtension dom stage = ULanguageExtension { _langExt :: String }

-- | An import declaration: @import Module.Name@
data UImportDecl dom stage
  = UImportDecl { _importSource :: AnnMaybeG UImportSource dom stage
                , _importQualified :: AnnMaybeG UImportQualified dom stage
                , _importSafe :: AnnMaybeG UImportSafe dom stage
                , _importPkg :: AnnMaybeG UStringNode dom stage
                , _importModule :: Ann UModuleName dom stage
                , _importAs :: AnnMaybeG UImportRenaming dom stage
                , _importSpec :: AnnMaybeG UImportSpec dom stage
                }

-- | Restriction on the imported names
data UImportSpec dom stage
  = UImportSpecList { _importSpecList :: AnnListG UIESpec dom stage
                    } -- ^ Restrict the import definition to ONLY import the listed names
  | UImportSpecHiding { _importSpecHiding :: AnnListG UIESpec dom stage
                      } -- ^ Restrict the import definition to DONT import the listed names

-- | Marks the import as qualified: @qualified@
data UImportQualified dom stage = UImportQualified

-- | Marks the import as source: @{-# SOURCE #-}@
data UImportSource dom stage = UImportSource

-- | Marks the import as safe: @safe@
data UImportSafe dom stage = UImportSafe

-- | Marks an imported name to belong to the type namespace: @type@
data UTypeNamespace dom stage = UTypeNamespace

-- | Renaming imports (@ as A @)
data UImportRenaming dom stage = UImportRenaming { _importRename :: Ann UModuleName dom stage }

-- | The name of a module
data UModuleName dom stage = UModuleName { _moduleNameString :: String }

-- | The @type@ keyword used to qualify that the type and not the constructor of the same name is referred
data TypeKeyword dom stage = TypeKeyword
