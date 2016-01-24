module Language.Haskell.Tools.AST.Module where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Decl

data Module a 
  = Module { _modHead :: AnnMaybe ModuleHead a
           , _modPragmas :: AnnList ModulePragma a
           , _modImports :: AnnList ImportDecl a
           , _modDecl :: AnnList Decl a
           }

-- | Module declaration with name and (optional) exports
data ModuleHead a
  = ModuleHead { _mhName :: Ann Name a
               , _mhExports :: AnnMaybe ExportSpecList a
               }

-- | A list of export specifications surrounded by parentheses
data ExportSpecList a
  = ExportSpecList { _espExports :: AnnList ExportSpec a }
  
data ExportSpec a
  = DeclExport { _exportDecl :: IESpec a }
  | ModuleExport { _exportModuleName :: Ann Name a } -- ^ The export of an imported module (@ module A @)
  
data IESpec a
  = IESpec { _ieName :: Ann Name a
           , _ieSubspec :: AnnMaybe SubSpec a
           } -- Import/export a declaration
  
data SubSpec a
  = SubSpecAll -- @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | SubSpecList { _essList :: AnnList Name a } -- @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
           
-- | Pragmas that affect the whole module           
data ModulePragma a
  = LanguagePragma { _lpPragmas :: AnnList Name a }  -- ^ LANGUAGE pragma
  | OptionsPragma { _opTool :: AnnMaybe Name a
                  , _opStr :: Ann StringNode a
                  } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
  | AnnModulePragma { _ampExpr :: Ann Expr a } -- ^ ANN pragma with module scope
                      
data ImportDecl a
  = ImportDecl { _importQualified :: AnnMaybe ImportQualified a
               , _importSource :: AnnMaybe ImportSource a
               , _importSafe :: AnnMaybe ImportSafe a
               , _importPkg :: AnnMaybe StringNode a
               , _importModule :: Ann Name a
               , _importAs :: AnnMaybe ImportRenaming a
               , _importSpec :: AnnMaybe ImportSpec a
               } -- ^ An import declaration
               
data ImportSpec a
  = ImportSpecList { _importSpecList :: AnnList IESpec a }
  | ImportSpecHiding { _importSpecList :: AnnList IESpec a } 
               
data ImportQualified a = ImportQualified
data ImportSource a = ImportSource
data ImportSafe a = ImportSafe
data TypeNamespace a = TypeNamespace

-- | Renaming imports (@ as A @)
data ImportRenaming a = ImportRenaming { _importRenamingName :: Ann Name a }
               