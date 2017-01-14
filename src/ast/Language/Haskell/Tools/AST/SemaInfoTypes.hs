{-# LANGUAGE DeriveDataTypeable
           , StandaloneDeriving 
           , TemplateHaskell 
           , UndecidableInstances
           , FlexibleContexts
           , FlexibleInstances
           #-}
module Language.Haskell.Tools.AST.SemaInfoTypes
  ( -- types 
    NoSemanticInfo, ScopeInfo, NameInfo, CNameInfo, ModuleInfo, ImportInfo, ImplicitFieldInfo
  , Scope
    -- references
  , exprScopedLocals, nameScopedLocals, nameIsDefined, nameInfo, ambiguousName, nameLocation
  , implicitName, cnameScopedLocals, cnameIsDefined, cnameInfo, cnameFixity
  , defModuleName, defIsBootModule, implicitNames, importedModule, availableNames, importedNames
  , implicitFieldBindings, importedOrphanInsts, importedFamInsts, prelOrphanInsts, prelFamInsts
    -- creator functions
  , mkNoSemanticInfo, mkScopeInfo, mkNameInfo, mkAmbiguousNameInfo, mkImplicitNameInfo, mkCNameInfo
  , mkModuleInfo, mkImportInfo, mkImplicitFieldInfo
  ) where

import Name as GHC
import BasicTypes as GHC
import Id as GHC
import Module as GHC
import SrcLoc as GHC
import RdrName as GHC
import Outputable as GHC
import InstEnv as GHC
import FamInstEnv as GHC

import Data.List
import Data.Data

import Control.Reference

type Scope = [[Name]]

-- | Semantic info type for any node not 
-- carrying additional semantic information
data NoSemanticInfo = NoSemanticInfo 
  deriving (Eq, Data)

mkNoSemanticInfo :: NoSemanticInfo
mkNoSemanticInfo = NoSemanticInfo

-- | Info for expressions that tells which definitions are in scope
data ScopeInfo = ScopeInfo { _exprScopedLocals :: Scope 
                           }
  deriving (Eq, Data)

-- | Creates the information about the definitions in scope
mkScopeInfo :: Scope -> ScopeInfo
mkScopeInfo = ScopeInfo

-- | Info corresponding to a name
data NameInfo n = NameInfo { _nameScopedLocals :: Scope
                           , _nameIsDefined :: Bool
                           , _nameInfo :: n
                           } 
                | AmbiguousNameInfo { _nameScopedLocals :: Scope
                                    , _nameIsDefined :: Bool
                                    , _ambiguousName :: RdrName
                                    , _nameLocation :: SrcSpan
                                    }
                | ImplicitNameInfo { _nameScopedLocals :: Scope
                                   , _nameIsDefined :: Bool
                                   , _implicitName :: String
                                   , _nameLocation :: SrcSpan
                                   }

  deriving (Eq, Data)

-- | Creates semantic information for an unambiguous name
mkNameInfo :: Scope -> Bool -> n -> NameInfo n
mkNameInfo = NameInfo

-- | Creates semantic information for a name that is ambiguous because the lack of type info
mkAmbiguousNameInfo :: Scope -> Bool -> RdrName -> SrcSpan -> NameInfo n
mkAmbiguousNameInfo = AmbiguousNameInfo

-- | Creates semantic information for an implicit name
mkImplicitNameInfo :: Scope -> Bool -> String -> SrcSpan -> NameInfo n
mkImplicitNameInfo = ImplicitNameInfo

-- | Info corresponding to a name that is correctly identified
data CNameInfo = CNameInfo { _cnameScopedLocals :: Scope
                           , _cnameIsDefined :: Bool
                           , _cnameInfo :: Id
                           , _cnameFixity :: Maybe GHC.Fixity
                           }
  deriving (Eq, Data)

-- | Create a typed name semantic information
mkCNameInfo :: Scope -> Bool -> Id -> Maybe GHC.Fixity -> CNameInfo
mkCNameInfo = CNameInfo

-- | Info for the module element
data ModuleInfo n = ModuleInfo { _defModuleName :: GHC.Module 
                               , _defIsBootModule :: Bool -- ^ True if this module is created from a hs-boot file
                               , _implicitNames :: [n] -- ^ Implicitely imported names
                               , _prelOrphanInsts :: [ClsInst] -- ^ Class instances implicitely passed from Prelude.
                               , _prelFamInsts :: [FamInst] -- ^ Family instances implicitely passed from Prelude.
                               } 
  deriving Data

-- | Creates semantic information for the module element
mkModuleInfo :: GHC.Module -> Bool -> [n] -> [ClsInst] -> [FamInst] -> ModuleInfo n
mkModuleInfo = ModuleInfo

-- | Info corresponding to an import declaration
data ImportInfo n = ImportInfo { _importedModule :: GHC.Module -- ^ The name and package of the imported module
                               , _availableNames :: [n] -- ^ Names available from the imported module
                               , _importedNames :: [n] -- ^ Names actually imported from the module.
                               , _importedOrphanInsts :: [ClsInst] -- ^ Class instances implicitely passed.
                               , _importedFamInsts :: [FamInst] -- ^ Family instances implicitely passed.
                               }
  deriving Data

deriving instance Data FamInst
deriving instance Data FamFlavor

-- | Creates semantic information for an import declaration
mkImportInfo :: GHC.Module -> [n] -> [n] -> [ClsInst] -> [FamInst] -> ImportInfo n
mkImportInfo = ImportInfo

-- | Info corresponding to an record-wildcard
data ImplicitFieldInfo = ImplicitFieldInfo { _implicitFieldBindings :: [(Name, Name)] -- ^ The implicitely bounded names
                                           } 
  deriving (Eq, Data)

-- | Creates semantic information for a wildcard field binding
mkImplicitFieldInfo :: [(Name, Name)] -> ImplicitFieldInfo
mkImplicitFieldInfo = ImplicitFieldInfo

instance Show ScopeInfo where
  show (ScopeInfo locals) = "(ScopeInfo " ++ showSDocUnsafe (ppr locals) ++ ")"

instance Outputable n => Show (NameInfo n) where
  show (NameInfo locals defined nameInfo) = "(NameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ ")"
  show (AmbiguousNameInfo locals defined nameInfo span) = "(AmbiguousNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ " " ++ show span ++ ")"
  show (ImplicitNameInfo locals defined nameInfo span) = "(ImplicitNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ " " ++ show span ++ ")"

instance Show CNameInfo where
  show (CNameInfo locals defined nameInfo fixity) = "(CNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ showSDocUnsafe (ppr fixity) ++ ")"

instance Outputable n => Show (ModuleInfo n) where
  show (ModuleInfo mod isboot imp clsInsts famInsts) 
    = "(ModuleInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ show isboot ++ " " ++ showSDocUnsafe (ppr imp) ++ " " 
          ++ showSDocUnsafe (ppr clsInsts) ++ " " ++ showSDocUnsafe (ppr famInsts) ++ ")"

instance Outputable n => Show (ImportInfo n) where
  show (ImportInfo mod avail imported clsInsts famInsts) 
    = "(ImportInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ showSDocUnsafe (ppr avail) ++ " " ++ showSDocUnsafe (ppr imported) ++ " " 
          ++ showSDocUnsafe (ppr clsInsts) ++ " " ++ showSDocUnsafe (ppr famInsts) ++ ")"

instance Show ImplicitFieldInfo where
  show (ImplicitFieldInfo bnds) = "(ImplicitFieldInfo [" ++ concat (intersperse "," (map (\(from,to) -> showSDocUnsafe (ppr from) ++ "->" ++ showSDocUnsafe (ppr to)) bnds)) ++ "])"

instance Show NoSemanticInfo where
  show NoSemanticInfo = "NoSemanticInfo"

makeReferences ''NoSemanticInfo
makeReferences ''ScopeInfo
makeReferences ''NameInfo
makeReferences ''CNameInfo
makeReferences ''ModuleInfo
makeReferences ''ImportInfo
makeReferences ''ImplicitFieldInfo

instance Functor NameInfo where
  fmap f = nameInfo .- f

instance Functor ModuleInfo where
  fmap f = implicitNames .- map f

instance Functor ImportInfo where
  fmap f (ImportInfo mod avail imps clsInsts famInsts) = ImportInfo mod (map f avail) (map f imps) clsInsts famInsts

instance Foldable NameInfo where
  foldMap f si = maybe mempty f (si ^? nameInfo)

instance Foldable ModuleInfo where
  foldMap f si = foldMap f (si ^. implicitNames)

instance Foldable ImportInfo where
  foldMap f si = foldMap f ((si ^. availableNames) ++ (si ^. importedNames))

instance Traversable NameInfo where
  traverse f (NameInfo locals defined nameInfo) = NameInfo locals defined <$> f nameInfo
  traverse _ (AmbiguousNameInfo locals defined nameInfo span) = pure $ AmbiguousNameInfo locals defined nameInfo span
  traverse _ (ImplicitNameInfo locals defined nameInfo span) = pure $ ImplicitNameInfo locals defined nameInfo span

instance Traversable ModuleInfo where
  traverse f (ModuleInfo mod isboot imp clsInsts famInsts) 
    = ModuleInfo mod isboot <$> traverse f imp <*> pure clsInsts <*> pure famInsts

instance Traversable ImportInfo where
  traverse f (ImportInfo mod avail imps clsInsts famInsts) 
    = ImportInfo mod <$> traverse f avail <*> traverse f imps <*> pure clsInsts <*> pure famInsts

