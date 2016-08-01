-- | Representation of Haskell modules with imports and exports
module Language.Haskell.Tools.AST.Modules where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Decls

data Module dom stage
  = Module { _filePragmas :: AnnList FilePragma dom stage
           , _modHead :: AnnMaybe ModuleHead dom stage
           , _modImports :: AnnList ImportDecl dom stage
           , _modDecl :: AnnList Decl dom stage
           }

-- | Module declaration with name and (optional) exports
data ModuleHead dom stage
  = ModuleHead { _mhName :: Ann ModuleName dom stage
               , _mhExports :: AnnMaybe ExportSpecList dom stage
               , _mhPragma :: AnnMaybe ModulePragma dom stage
               }

-- | A list of export specifications surrounded by parentheses
data ExportSpecList dom stage
  = ExportSpecList { _espExports :: AnnList ExportSpec dom stage }
  
-- | Export specifier
data ExportSpec dom stage
  = DeclExport { _exportDecl :: Ann IESpec dom stage
               } -- ^ Export a name and related names
  | ModuleExport { _exportModuleName :: Ann ModuleName dom stage
                 } -- ^ The export of an imported module (@ module A @)
  
-- | Marks a name to be imported or exported with related names (subspecifier)
data IESpec dom stage
  = IESpec { _ieName :: Ann Name dom stage
           , _ieSubspec :: AnnMaybe SubSpec dom stage
           }
  
-- | Marks how related names will be imported or exported with a given name
data SubSpec dom stage
  = SubSpecAll -- @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | SubSpecList { _essList :: AnnList Name dom stage } -- @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
           
-- | Pragmas that must be used before defining the module         
data FilePragma dom stage
  = LanguagePragma { _lpPragmas :: AnnList LanguageExtension dom stage
                   }  -- ^ LANGUAGE pragmdom stage
  | OptionsPragma {  _opStr :: Ann StringNode dom stage
                  } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
                        
-- | Pragmas that must be used after the module head  
data ModulePragma dom stage
  = ModuleWarningPragma { _modWarningStr :: AnnList StringNode dom stage
                        }  -- ^ a warning pragma attached to the module
  | ModuleDeprecatedPragma {  _modDeprecatedPragma :: AnnList StringNode dom stage
                           } -- ^ a deprecated pragma attached to the module
             
data LanguageExtension dom stage = LanguageExtension { _langExt :: String }

-- | An import declaration: @import Module.Name@         
data ImportDecl dom stage
  = ImportDecl { _importSource :: AnnMaybe ImportSource dom stage
               , _importQualified :: AnnMaybe ImportQualified dom stage
               , _importSafe :: AnnMaybe ImportSafe dom stage
               , _importPkg :: AnnMaybe StringNode dom stage
               , _importModule :: Ann ModuleName dom stage
               , _importAs :: AnnMaybe ImportRenaming dom stage
               , _importSpec :: AnnMaybe ImportSpec dom stage
               }

-- | Restriction on the imported names
data ImportSpec dom stage
  = ImportSpecList { _importSpecList :: AnnList IESpec dom stage
                   } -- ^ Restrict the import definition to ONLY import the listed names
  | ImportSpecHiding { _importSpecHiding :: AnnList IESpec dom stage
                     } -- ^ Restrict the import definition to DONT import the listed names
               
-- | Marks the import as qualified: @qualified@
data ImportQualified dom stage = ImportQualified

-- | Marks the import as source: @{-# SOURCE #-}@
data ImportSource dom stage = ImportSource

-- | Marks the import as safe: @safe@
data ImportSafe dom stage = ImportSafe

-- | Marks an imported name to belong to the type namespace: @type@
data TypeNamespace dom stage = TypeNamespace

-- | Renaming imports (@ as A @)
data ImportRenaming dom stage = ImportRenaming { _importRename :: Ann ModuleName dom stage }