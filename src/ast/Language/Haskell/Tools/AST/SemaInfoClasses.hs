{-# LANGUAGE FlexibleInstances, FlexibleContexts, ConstraintKinds #-}
module Language.Haskell.Tools.AST.SemaInfoClasses where

import GHC
import Id as GHC
import BasicTypes as GHC

import Control.Reference

import Language.Haskell.Tools.AST.Ann as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST
import Language.Haskell.Tools.AST.Modules as AST
import Language.Haskell.Tools.AST.Base as AST
import Language.Haskell.Tools.AST.Exprs as AST

-- | Domains that have semantic information for names
type HasNameInfo dom = (Domain dom, HasNameInfo' (SemanticInfo dom QualifiedName))

-- | Infos that may have a name that can be extracted
class HasNameInfo' si where
  semanticsName :: si -> Maybe GHC.Name

instance HasNameInfo' (NameInfo GHC.Name) where
  semanticsName = (^? nameInfo)

instance HasNameInfo' CNameInfo where
  semanticsName = fmap idName . (^? cnameInfo)

type HasIdInfo dom = (Domain dom, HasIdInfo' (SemanticInfo dom QualifiedName))

-- | Infos that may have a typed name that can be extracted
class HasIdInfo' si where
  semanticsId :: si -> Id

instance HasIdInfo' CNameInfo where
  semanticsId = (^. cnameInfo)

type HasFixityInfo dom = (Domain dom, HasFixityInfo' (SemanticInfo dom QualifiedName))

-- | Infos that may have a fixity information
class HasFixityInfo' si where
  semanticsFixity :: si -> Maybe GHC.Fixity

instance HasFixityInfo' CNameInfo where
  semanticsFixity = (^. cnameFixity)

type HasScopeInfo dom = (Domain dom, HasScopeInfo' (SemanticInfo dom QualifiedName), HasScopeInfo' (SemanticInfo dom Expr))

-- | Infos that contain the names that are available in theirs scope
class HasScopeInfo' si where
  semanticsScope :: si -> Scope

instance HasScopeInfo' (NameInfo n) where
  semanticsScope = (^. nameScopedLocals)

instance HasScopeInfo' CNameInfo where
  semanticsScope = (^. cnameScopedLocals)

instance HasScopeInfo' ScopeInfo where
  semanticsScope = (^. exprScopedLocals)

type HasDefiningInfo dom = (Domain dom, HasDefiningInfo' (SemanticInfo dom QualifiedName))

-- | Infos that store if they were used to define a name
class HasDefiningInfo' si where
  semanticsDefining :: si -> Bool

instance HasDefiningInfo' (NameInfo n) where
  semanticsDefining = (^. nameIsDefined)

instance HasDefiningInfo' CNameInfo where
  semanticsDefining = (^. cnameIsDefined)

type HasModuleInfo dom = (Domain dom, HasModuleInfo' (SemanticInfo dom AST.Module))

class HasModuleInfo' si where
  semanticsModule :: si -> GHC.Module
  isBootModule :: si -> Bool
  semanticsImplicitImports :: si -> [GHC.Name]

instance HasModuleInfo' (AST.ModuleInfo GHC.Name) where
  semanticsModule = (^. defModuleName)
  isBootModule = (^. defIsBootModule)
  semanticsImplicitImports = (^. implicitNames)

instance HasModuleInfo' (AST.ModuleInfo GHC.Id) where
  semanticsModule = (^. defModuleName)
  isBootModule = (^. defIsBootModule)
  semanticsImplicitImports = map idName . (^. implicitNames)

type HasImportInfo dom = (Domain dom, HasImportInfo' (SemanticInfo dom AST.ImportDecl))
  
class HasImportInfo' si where
  semanticsImportedModule :: si -> GHC.Module
  semanticsAvailable :: si -> [GHC.Name]
  semanticsImported :: si -> [GHC.Name]

instance HasImportInfo' (AST.ImportInfo GHC.Name) where
  semanticsImportedModule = (^. importedModule)
  semanticsAvailable = (^. availableNames)
  semanticsImported = (^. importedNames)

instance HasImportInfo' (AST.ImportInfo GHC.Id) where
  semanticsImportedModule = (^. importedModule)
  semanticsAvailable = map idName . (^. availableNames)
  semanticsImported = map idName . (^. importedNames)

type HasImplicitFieldsInfo dom = (Domain dom, HasImplicitFieldsInfo' (SemanticInfo dom AST.FieldWildcard))
  
class HasImplicitFieldsInfo' si where
  semanticsImplicitFlds :: si -> [(GHC.Name, GHC.Name)]

instance HasImplicitFieldsInfo' ImplicitFieldInfo where
  semanticsImplicitFlds = (^. implicitFieldBindings)
