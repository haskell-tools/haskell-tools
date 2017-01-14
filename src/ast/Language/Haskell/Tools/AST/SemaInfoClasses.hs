{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , ConstraintKinds
           , TypeFamilies
           , UndecidableInstances
           #-}
module Language.Haskell.Tools.AST.SemaInfoClasses where

import GHC
import Id as GHC

import Control.Reference

import Language.Haskell.Tools.AST.Ann as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST
import Language.Haskell.Tools.AST.Representation.Modules as AST
import Language.Haskell.Tools.AST.Representation.Names as AST
import Language.Haskell.Tools.AST.Representation.Exprs as AST

-- * Information about names

-- | Domains that have semantic information for names
type HasNameInfo dom = (Domain dom, HasNameInfo' (SemanticInfo dom UQualifiedName))

-- | Infos that may have a name that can be extracted
class HasNameInfo' si where
  semanticsName :: si -> Maybe GHC.Name

instance HasNameInfo' (NameInfo GHC.Name) where
  semanticsName = (^? nameInfo)

instance HasNameInfo' CNameInfo where
  semanticsName = fmap idName . (^? cnameInfo)

instance HasNameInfo dom => HasNameInfo' (Ann UQualifiedName dom st) where
  semanticsName = semanticsName . (^. annotation&semanticInfo) 

-- * Information about typed names

type HasIdInfo dom = (Domain dom, HasIdInfo' (SemanticInfo dom UQualifiedName))

-- | Infos that may have a typed name that can be extracted
class HasNameInfo' si => HasIdInfo' si where
  semanticsId :: si -> Id

instance HasIdInfo' CNameInfo where
  semanticsId = (^. cnameInfo)

instance HasIdInfo dom => HasIdInfo' (Ann UQualifiedName dom st) where
  semanticsId = semanticsId . (^. annotation&semanticInfo) 

-- * Fixity information

type HasFixityInfo dom = (Domain dom, HasFixityInfo' (SemanticInfo dom UQualifiedName))

-- | Infos that may have a fixity information
class HasFixityInfo' si where
  semanticsFixity :: si -> Maybe GHC.Fixity

instance HasFixityInfo' CNameInfo where
  semanticsFixity = (^. cnameFixity)

instance HasFixityInfo dom => HasFixityInfo' (Ann UQualifiedName dom st) where
  semanticsFixity = semanticsFixity . (^. annotation&semanticInfo) 

-- * Scope information

type HasScopeInfo dom = (Domain dom, HasScopeInfo' (SemanticInfo dom UQualifiedName), HasScopeInfo' (SemanticInfo dom UExpr))

-- | Infos that contain the names that are available in theirs scope
class HasScopeInfo' si where
  semanticsScope :: si -> Scope

instance HasScopeInfo' (NameInfo n) where
  semanticsScope = (^. nameScopedLocals)

instance HasScopeInfo' CNameInfo where
  semanticsScope = (^. cnameScopedLocals)

instance HasScopeInfo' ScopeInfo where
  semanticsScope = (^. exprScopedLocals)

instance HasScopeInfo dom => HasScopeInfo' (Ann UExpr dom st) where
  semanticsScope = semanticsScope . (^. annotation&semanticInfo) 

instance HasScopeInfo dom => HasScopeInfo' (Ann UQualifiedName dom st) where
  semanticsScope = semanticsScope . (^. annotation&semanticInfo) 

-- * Information about names being defined

type HasDefiningInfo dom = (Domain dom, HasDefiningInfo' (SemanticInfo dom UQualifiedName))

-- | Infos that store if they were used to define a name
class HasDefiningInfo' si where
  semanticsDefining :: si -> Bool

instance HasDefiningInfo' (NameInfo n) where
  semanticsDefining = (^. nameIsDefined)

instance HasDefiningInfo' CNameInfo where
  semanticsDefining = (^. cnameIsDefined)

instance HasDefiningInfo dom => HasDefiningInfo' (Ann UQualifiedName dom st) where
  semanticsDefining = semanticsDefining . (^. annotation&semanticInfo) 

-- * Information about source info in sema

class HasSourceInfoInSema' si where
  semanticsSourceInfo :: si -> Maybe SrcSpan

instance HasSourceInfoInSema' (NameInfo n) where
  semanticsSourceInfo = (^? nameLocation)

-- * Information about modules

type HasModuleInfo dom = (Domain dom, HasModuleInfo' (SemanticInfo dom AST.UModule))

class HasModuleInfo' si where
  semanticsModule :: si -> GHC.Module
  isBootModule :: si -> Bool
  semanticsImplicitImports :: si -> [GHC.Name]
  semanticsPrelOrphanInsts :: si -> [ClsInst]
  semanticsPrelFamInsts :: si -> [FamInst]

instance HasModuleInfo' (AST.ModuleInfo GHC.Name) where
  semanticsModule = (^. defModuleName)
  isBootModule = (^. defIsBootModule)
  semanticsImplicitImports = (^. implicitNames)
  semanticsPrelOrphanInsts = (^. prelOrphanInsts)
  semanticsPrelFamInsts = (^. prelFamInsts)

instance HasModuleInfo' (AST.ModuleInfo GHC.Id) where
  semanticsModule = (^. defModuleName)
  isBootModule = (^. defIsBootModule)
  semanticsImplicitImports = map idName . (^. implicitNames)
  semanticsPrelOrphanInsts = (^. prelOrphanInsts)
  semanticsPrelFamInsts = (^. prelFamInsts)

instance HasModuleInfo dom => HasModuleInfo' (Ann UModule dom st) where
  semanticsModule = semanticsModule . (^. annotation&semanticInfo) 
  isBootModule = isBootModule . (^. annotation&semanticInfo) 
  semanticsImplicitImports = semanticsImplicitImports . (^. annotation&semanticInfo) 
  semanticsPrelOrphanInsts = semanticsPrelOrphanInsts . (^. annotation&semanticInfo) 
  semanticsPrelFamInsts = semanticsPrelFamInsts . (^. annotation&semanticInfo) 

-- * Information about imports

type HasImportInfo dom = (Domain dom, HasImportInfo' (SemanticInfo dom AST.UImportDecl))
  
class HasImportInfo' si where
  semanticsImportedModule :: si -> GHC.Module
  semanticsAvailable :: si -> [GHC.Name]
  semanticsImported :: si -> [GHC.Name]
  semanticsOrphanInsts :: si -> [ClsInst]
  semanticsFamInsts :: si -> [FamInst]

instance HasImportInfo' (AST.ImportInfo GHC.Name) where
  semanticsImportedModule = (^. importedModule)
  semanticsAvailable = (^. availableNames)
  semanticsImported = (^. importedNames)
  semanticsOrphanInsts = (^. importedOrphanInsts)
  semanticsFamInsts = (^. importedFamInsts)

instance HasImportInfo' (AST.ImportInfo GHC.Id) where
  semanticsImportedModule = (^. importedModule)
  semanticsAvailable = map idName . (^. availableNames)
  semanticsImported = map idName . (^. importedNames)
  semanticsOrphanInsts = (^. importedOrphanInsts)
  semanticsFamInsts = (^. importedFamInsts)

instance HasImportInfo dom => HasImportInfo' (Ann UImportDecl dom st) where
  semanticsImportedModule = semanticsImportedModule . (^. annotation&semanticInfo) 
  semanticsAvailable = semanticsAvailable . (^. annotation&semanticInfo) 
  semanticsImported = semanticsImported . (^. annotation&semanticInfo) 
  semanticsOrphanInsts = semanticsOrphanInsts . (^. annotation&semanticInfo) 
  semanticsFamInsts = semanticsFamInsts . (^. annotation&semanticInfo) 

-- * Information about implicitely bounded fields

type HasImplicitFieldsInfo dom = (Domain dom, HasImplicitFieldsInfo' (SemanticInfo dom AST.UFieldWildcard))
  
class HasImplicitFieldsInfo' si where
  semanticsImplicitFlds :: si -> [(GHC.Name, GHC.Name)]

instance HasImplicitFieldsInfo' ImplicitFieldInfo where
  semanticsImplicitFlds = (^. implicitFieldBindings)

instance HasImplicitFieldsInfo dom => HasImplicitFieldsInfo' (Ann UFieldWildcard dom st) where
  semanticsImplicitFlds = semanticsImplicitFlds . (^. annotation&semanticInfo) 

-- * AST elements with no information

type HasNoSemanticInfo dom si = SemanticInfo dom si ~ NoSemanticInfo
