{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
module Language.Haskell.Tools.AST.SemaInfoTypes
  ( -- types
    NoSemanticInfo, ScopeInfo, NameInfo, CNameInfo, ModuleInfo, ImportInfo, ImplicitFieldInfo
  , Scope, UsageSpec(..), LiteralInfo(..), PreLiteralInfo(..)
    -- references
  , exprScopedLocals, nameScopedLocals, nameIsDefined, nameInfo, ambiguousName, nameLocation
  , implicitName, cnameScopedLocals, cnameIsDefined, cnameInfo, cnameFixity
  , defModuleName, defDynFlags, defIsBootModule, implicitNames, importedModule, availableNames
  , importedNames, implicitFieldBindings, importedOrphanInsts, importedFamInsts, prelOrphanInsts
  , prelFamInsts, literalType
    -- creator functions
  , mkNoSemanticInfo, mkScopeInfo, mkNameInfo, mkAmbiguousNameInfo, mkImplicitNameInfo, mkCNameInfo
  , mkModuleInfo, mkImportInfo, mkImplicitFieldInfo
  -- utils
  , PName(..), pName, pNameParent
  ) where

import BasicTypes as GHC
import DynFlags as GHC
import FamInstEnv as GHC
import Id as GHC
import InstEnv as GHC
import Module as GHC
import Name as GHC
import Outputable as GHC
import RdrName as GHC
import SrcLoc as GHC
import Type as GHC

import Data.Data as Data
import Data.List

import Control.Reference

type Scope = [[(Name, Maybe [UsageSpec], Maybe Name)]]

data UsageSpec = UsageSpec { usageQualified :: Bool
                           , usageQualifier :: String
                           , usageAs :: String
                           }
  deriving (Eq, Data)

instance Outputable UsageSpec where
  ppr (UsageSpec q useQ asQ)
    = GHC.text $ (if q then "qualified " else "") ++ "as "
        ++ (if useQ == asQ || q then asQ else asQ ++ " or " ++ useQ)
  pprPrec _ (UsageSpec q useQ asQ)
    = GHC.text $ (if q then "qualified " else "") ++ "as "
        ++ (if useQ == asQ || q then asQ else asQ ++ " or " ++ useQ)

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

data PreLiteralInfo = RealLiteralInfo { _realLiteralType :: Type
                                      }
                    | PreLiteralInfo { _preLiteralLoc :: SrcSpan
                                     }
  deriving (Data)

instance Show PreLiteralInfo where
  show (RealLiteralInfo t) = "RealLiteralInfo (" ++ showSDocUnsafe (ppr t) ++ ")"
  show (PreLiteralInfo sp) = "PreLiteralInfo (" ++ show sp ++ ")"

data LiteralInfo = LiteralInfo { _literalType :: Type
                               }
  deriving (Data)

instance Show LiteralInfo where
  show (LiteralInfo t) = "LiteralInfo (" ++ showSDocUnsafe (ppr t) ++ ")"


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

data PName n
  = PName { _pName :: n
          , _pNameParent :: Maybe n
          }
  deriving Data

-- | Info for the module element
data ModuleInfo n = ModuleInfo { _defModuleName :: GHC.Module
                               , _defDynFlags :: DynFlags -- ^ The compilation flags that are set up when the module was compiled
                               , _defIsBootModule :: Bool -- ^ True if this module is created from a hs-boot file
                               , _implicitNames :: [PName n] -- ^ implicitly imported names
                               , _prelOrphanInsts :: [ClsInst] -- ^ Class instances implicitly passed from Prelude.
                               , _prelFamInsts :: [FamInst] -- ^ Family instances implicitly passed from Prelude.
                               }
  deriving Data

instance Data DynFlags where
  gunfold _ _ _ = error "Cannot construct dyn flags"
  toConstr _ = dynFlagsCon
  dataTypeOf _ = dynFlagsType

dynFlagsType = mkDataType "DynFlags.DynFlags" [dynFlagsCon]
dynFlagsCon = mkConstr dynFlagsType "DynFlags" [] Data.Prefix

-- | Creates semantic information for the module element.
-- Strict in the list of implicitely imported, orphan and family instances.
mkModuleInfo :: GHC.Module -> DynFlags -> Bool -> [PName n] -> [ClsInst] -> [FamInst] -> ModuleInfo n
-- the calculate of these fields involves a big parts of the GHC state and it causes a space leak
-- if not evaluated strictly
mkModuleInfo mod dfs boot !imported !orphan !family = ModuleInfo mod dfs boot imported orphan family

-- | Info corresponding to an import declaration
data ImportInfo n = ImportInfo { _importedModule :: GHC.Module -- ^ The name and package of the imported module
                               , _availableNames :: [n] -- ^ Names available from the imported module
                               , _importedNames :: [PName n] -- ^ Names actually imported from the module.
                               , _importedOrphanInsts :: [ClsInst] -- ^ Class instances implicitly passed.
                               , _importedFamInsts :: [FamInst] -- ^ Family instances implicitly passed.
                               }
  deriving Data

deriving instance Data FamInst
deriving instance Data FamFlavor

-- | Creates semantic information for an import declaration
-- Strict in the list of the used and imported declarations, orphan and family instances.
mkImportInfo :: GHC.Module -> [n] -> [PName n] -> [ClsInst] -> [FamInst] -> ImportInfo n
-- the calculate of these fields involves a big parts of the GHC state and it causes a space leak
-- if not evaluated strictly
mkImportInfo mod !names !imported !orphan !family = ImportInfo mod names imported orphan family

-- | Info corresponding to an record-wildcard
data ImplicitFieldInfo = ImplicitFieldInfo { _implicitFieldBindings :: [(Name, Name)] -- ^ The implicitly bounded names
                                           }
  deriving (Eq, Data)

-- | Creates semantic information for a wildcard field binding
mkImplicitFieldInfo :: [(Name, Name)] -> ImplicitFieldInfo
mkImplicitFieldInfo = ImplicitFieldInfo

instance Show ScopeInfo where
  show (ScopeInfo locals) = "(ScopeInfo " ++ showSDocUnsafe (ppr locals) ++ ")"

instance Outputable n => Show (NameInfo n) where
  show (NameInfo locals defined nameInfo)
    = "(NameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " "
        ++ showSDocUnsafe (ppr nameInfo) ++ ")"
  show (AmbiguousNameInfo locals defined nameInfo span)
    = "(AmbiguousNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " "
        ++ showSDocUnsafe (ppr nameInfo) ++ " " ++ show span ++ ")"
  show (ImplicitNameInfo locals defined nameInfo span)
    = "(ImplicitNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " "
        ++ showSDocUnsafe (ppr nameInfo) ++ " " ++ show span ++ ")"

instance Show CNameInfo where
  show (CNameInfo locals defined nameInfo fixity)
    = "(CNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " "
        ++ showSDocUnsafe (ppr nameInfo) ++ showSDocUnsafe (ppr fixity) ++ ")"

instance Outputable n => Show (PName n) where
  show (PName n (Just parent))
    = showSDocUnsafe (ppr n) ++ "[in " ++ showSDocUnsafe (ppr parent) ++ "]"
  show (PName n Nothing) = showSDocUnsafe (ppr n)

instance Outputable n => Show (ModuleInfo n) where
  show (ModuleInfo mod _ isboot imp clsInsts famInsts)
    = "(ModuleInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ show isboot ++ " " ++ show imp ++ " "
          ++ showSDocUnsafe (ppr clsInsts) ++ " " ++ showSDocUnsafe (ppr famInsts) ++ ")"

instance Outputable n => Show (ImportInfo n) where
  show (ImportInfo mod avail imported clsInsts famInsts)
    = "(ImportInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ showSDocUnsafe (ppr avail) ++ " "
        ++ show imported ++ " " ++ showSDocUnsafe (ppr clsInsts) ++ " "
        ++ showSDocUnsafe (ppr famInsts) ++ ")"

instance Show ImplicitFieldInfo where
  show (ImplicitFieldInfo bnds)
    = "(ImplicitFieldInfo [" ++ concat (intersperse "," (map showImplicitFld bnds)) ++ "])"
    where showImplicitFld (from, to) = showSDocUnsafe (ppr from) ++ "->" ++ showSDocUnsafe (ppr to)

instance Show NoSemanticInfo where
  show NoSemanticInfo = "NoSemanticInfo"

makeReferences ''PName
makeReferences ''NoSemanticInfo
makeReferences ''ScopeInfo
makeReferences ''NameInfo
makeReferences ''CNameInfo
makeReferences ''ModuleInfo
makeReferences ''ImportInfo
makeReferences ''ImplicitFieldInfo
makeReferences ''LiteralInfo

instance Functor NameInfo where
  fmap f = nameInfo .- f

instance Functor PName where
  fmap f (PName n p) = PName (f n) (fmap f p)

instance Functor ModuleInfo where
  fmap f = implicitNames .- fmap (fmap f)

instance Functor ImportInfo where
  fmap f (ImportInfo mod avail imps clsInsts famInsts)
    = ImportInfo mod (fmap f avail) (fmap (fmap f) imps) clsInsts famInsts

instance Foldable NameInfo where
  foldMap f si = maybe mempty f (si ^? nameInfo)

instance Foldable ModuleInfo where
  foldMap f si = foldMap (foldMap f) (si ^. implicitNames)

instance Foldable ImportInfo where
  foldMap f si = foldMap f (((si ^. availableNames)
                   ++ (si ^? importedNames & traversal & (pName &+& pNameParent & just) )))

instance Foldable PName where
  foldMap f (PName n p) = f n `mappend` foldMap f p

instance Traversable PName where
  traverse f (PName n p) = PName <$> f n <*> traverse f p

instance Traversable NameInfo where
  traverse f (NameInfo locals defined nameInfo) = NameInfo locals defined <$> f nameInfo
  traverse _ (AmbiguousNameInfo locals defined nameInfo span)
    = pure $ AmbiguousNameInfo locals defined nameInfo span
  traverse _ (ImplicitNameInfo locals defined nameInfo span)
    = pure $ ImplicitNameInfo locals defined nameInfo span

instance Traversable ModuleInfo where
  traverse f (ModuleInfo mod dfs isboot imp clsInsts famInsts)
    = ModuleInfo mod dfs isboot <$> traverse (traverse f) imp <*> pure clsInsts <*> pure famInsts

instance Traversable ImportInfo where
  traverse f (ImportInfo mod avail imps clsInsts famInsts)
    = ImportInfo mod <$> traverse f avail <*> traverse (traverse f) imps <*> pure clsInsts
                     <*> pure famInsts
