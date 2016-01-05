module Language.Haskell.Tools.AST.Module where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Decl

data Module a 
  = Module { modHead    :: AnnMaybe ModuleHead a
           , modPragmas :: AnnList ModulePragma a
           , modImports :: AnnList ImportDecl a
           , modDecl    :: AnnList Decl a
           }

-- | Module declaration with name and (optional) exports
data ModuleHead a
  = ModuleHead { mhName    :: Ann Name a
               , mhExports :: AnnMaybe ExportSpecList a
               }

-- | A list of export specifications surrounded by parentheses
data ExportSpecList a
  = ExportSpecList { espExports :: AnnList (AnnEither IESpec ExportModule) a }
  
data IESpec a
  = IESpec { ieName    :: Ann Name a
           , ieSubspec :: AnnMaybe SubSpec a
           } -- Import/export a declaration

-- | The export of an imported module (@ module A @)
data ExportModule a
  = ExportModule { emName :: Ann Name a }
  
data SubSpec a
  = SubSpecAll -- @T(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | SubSpecList { essList :: AnnList Name a } -- @T(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
           
-- | Pragmas that affect the whole module           
data ModulePragma a
  = LanguagePragma { lpPragmas :: AnnList Name a }  -- ^ LANGUAGE pragma
  | OptionsPragma { opTool :: AnnMaybe Name a
                  , opStr :: Ann StringNode a
                  } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
  | AnnModulePragma { ampExpr  :: Ann Expr a } -- ^ ANN pragma with module scope
                      
data ImportDecl a
  = ImportDecl { importQualified    :: AnnMaybe ImportQualified a
               , importSource       :: AnnMaybe ImportSource a
               , importSafe         :: AnnMaybe ImportSafe a
               , importPkg          :: AnnMaybe StringNode a
               , importModule       :: Ann Name a
               , importAs           :: AnnMaybe ImportRenaming a
               , importSpec         :: AnnMaybe ImportSpec a
               } -- ^ An import declaration
               
data ImportSpec a
  = ImportSpecList { importSpecList :: AnnList IESpec a }
  | ImportSpecHiding { importSpecList :: AnnList IESpec a } 
               
data ImportQualified a = ImportQualified
data ImportSource a = ImportSource
data ImportSafe a = ImportSafe
data TypeNamespace a = TypeNamespace

-- | Renaming imports (@ as A @)
data ImportRenaming a = ImportRenaming { importRenamingName :: Ann Name a }
               