{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances, UndecidableInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
import RdrName as GHC
import SrcLoc as GHC
import Type as GHC

import Data.Data as Data
import Control.Reference

type Scope = [[(Name, Maybe [UsageSpec], Maybe Name)]]

data UsageSpec = UsageSpec { usageQualified :: Bool
                           , usageQualifier :: String
                           , usageAs :: String
                           }
  deriving Data

-- | Semantic info type for any node not
-- carrying additional semantic information
data NoSemanticInfo = NoSemanticInfo
  deriving Data

mkNoSemanticInfo :: NoSemanticInfo
mkNoSemanticInfo = NoSemanticInfo

-- | Info for expressions that tells which definitions are in scope
data ScopeInfo = ScopeInfo { _exprScopedLocals :: Scope
                           }
  deriving Data

-- | Creates the information about the definitions in scope
mkScopeInfo :: Scope -> ScopeInfo
mkScopeInfo = ScopeInfo

data PreLiteralInfo = RealLiteralInfo { _realLiteralType :: Type
                                      }
                    | PreLiteralInfo { _preLiteralLoc :: SrcSpan
                                     }
  deriving Data

data LiteralInfo = LiteralInfo { _literalType :: Type
                               }
  deriving Data

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

  deriving (Data, Functor, Foldable, Traversable)

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
  deriving Data

-- | Create a typed name semantic information
mkCNameInfo :: Scope -> Bool -> Id -> Maybe GHC.Fixity -> CNameInfo
mkCNameInfo = CNameInfo

data PName n
  = PName { _pName :: n
          , _pNameParent :: Maybe n
          }
  deriving (Data, Functor, Foldable, Traversable)

-- | Info for the module element
data ModuleInfo n = ModuleInfo { _defModuleName :: GHC.Module
                               , _defDynFlags :: DynFlags -- ^ The compilation flags that are set up when the module was compiled
                               , _defIsBootModule :: Bool -- ^ True if this module is created from a hs-boot file
                               , _implicitNames :: [PName n] -- ^ implicitly imported names
                               , _prelOrphanInsts :: [ClsInst] -- ^ Class instances implicitly passed from Prelude.
                               , _prelFamInsts :: [FamInst] -- ^ Family instances implicitly passed from Prelude.
                               }
  deriving (Data, Functor, Foldable, Traversable)

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
  deriving (Data, Functor, Foldable, Traversable)

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
  deriving Data

-- | Creates semantic information for a wildcard field binding
mkImplicitFieldInfo :: [(Name, Name)] -> ImplicitFieldInfo
mkImplicitFieldInfo = ImplicitFieldInfo

makeReferences ''PName
makeReferences ''ScopeInfo
makeReferences ''NameInfo
makeReferences ''CNameInfo
makeReferences ''ModuleInfo
makeReferences ''ImportInfo
makeReferences ''ImplicitFieldInfo
makeReferences ''LiteralInfo
