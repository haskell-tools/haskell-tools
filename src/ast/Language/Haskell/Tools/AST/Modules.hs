-- | Representation of Haskell modules with imports and exports
module Language.Haskell.Tools.AST.Modules where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Decls

data Module a 
  = Module { _filePragmas :: AnnList FilePragma a
           , _modHead :: AnnMaybe ModuleHead a
           , _modImports :: AnnList ImportDecl a
           , _modDecl :: AnnList Decl a
           }

-- | Module declaration with name and (optional) exports
data ModuleHead a
  = ModuleHead { _mhName :: Ann SimpleName a
               , _mhExports :: AnnMaybe ExportSpecList a
               , _mhPragma :: AnnMaybe ModulePragma a
               }

-- | A list of export specifications surrounded by parentheses
data ExportSpecList a
  = ExportSpecList { _espExports :: AnnList ExportSpec a }
  
-- | Export specifier
data ExportSpec a
  = DeclExport { _exportDecl :: Ann IESpec a 
               } -- ^ Export a name and related names
  | ModuleExport { _exportModuleName :: Ann SimpleName a 
                 } -- ^ The export of an imported module (@ module A @)
  
-- | Marks a name to be imported or exported with related names (subspecifier)
data IESpec a
  = IESpec { _ieName :: Ann Name a
           , _ieSubspec :: AnnMaybe SubSpec a
           }
  
-- | Marks how related names will be imported or exported with a given name
data SubSpec a
  = SubSpecAll -- @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | SubSpecList { _essList :: AnnList Name a } -- @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
           
-- | Pragmas that must be used before defining the module         
data FilePragma a
  = LanguagePragma { _lpPragmas :: AnnList LanguageExtension a 
                   }  -- ^ LANGUAGE pragma
  | OptionsPragma {  _opStr :: Ann StringNode a
                  } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
                        
-- | Pragmas that must be used after the module head  
data ModulePragma a
  = ModuleWarningPragma { _modWarningStr :: AnnList StringNode a 
                        }  -- ^ a warning pragma attached to the module
  | ModuleDeprecatedPragma {  _modDeprecatedPragma :: AnnList StringNode a
                           } -- ^ a deprecated pragma attached to the module
             
data LanguageExtension a = LanguageExtension { _langExt :: String }

-- | An import declaration: @import Module.Name@         
data ImportDecl a
  = ImportDecl { _importSource :: AnnMaybe ImportSource a
               , _importQualified :: AnnMaybe ImportQualified a
               , _importSafe :: AnnMaybe ImportSafe a
               , _importPkg :: AnnMaybe StringNode a
               , _importModule :: Ann SimpleName a
               , _importAs :: AnnMaybe ImportRenaming a
               , _importSpec :: AnnMaybe ImportSpec a
               }

-- | Restriction on the imported names
data ImportSpec a
  = ImportSpecList { _importSpecList :: AnnList IESpec a 
                   } -- ^ Restrict the import definition to ONLY import the listed names
  | ImportSpecHiding { _importSpecHiding :: AnnList IESpec a 
                     } -- ^ Restrict the import definition to DONT import the listed names
               
-- | Marks the import as qualified: @qualified@
data ImportQualified a = ImportQualified

-- | Marks the import as source: @{-# SOURCE #-}@
data ImportSource a = ImportSource

-- | Marks the import as safe: @safe@
data ImportSafe a = ImportSafe

-- | Marks an imported name to belong to the type namespace: @type@
data TypeNamespace a = TypeNamespace

-- | Renaming imports (@ as A @)
data ImportRenaming a = ImportRenaming { _importRename :: Ann SimpleName a }
               