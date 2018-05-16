{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances, UndecidableInstances, DeriveTraversable #-}

module Language.Haskell.Tools.AST.SemaInfoTypes
  ( -- types
    NoSemanticInfo, ScopeInfo, NameInfo, CNameInfo, ModuleInfo, ImportInfo, ImplicitFieldInfo
  , Scope, UsageSpec(..), LiteralInfo(..), PreLiteralInfo(..)
    -- references
  , exprScopedLocals, nameScopedLocals, nameIsDefined, nameInfo, ambiguousName, nameLocation
  , implicitName, cnameScopedLocals, cnameIsDefined, cnameInfo, cnameFixity
  , defModuleName, defDynFlags, defIsBootModule, implicitNames, importedModule, availableNames
  , importedNames, implicitFieldBindings, prelTransMods, importTransMods, literalType
    -- creator functions
  , mkNoSemanticInfo, mkScopeInfo, mkNameInfo, mkAmbiguousNameInfo, mkImplicitNameInfo, mkCNameInfo
  , mkModuleInfo, mkImportInfo, mkImplicitFieldInfo
  -- utils
  , PName(..), pName, pNameParent, trfPNames, trfPNamesM, trfImportInfo, trfImportInfoM, trfModuleInfoM
  , getInstances
  ) where

import BasicTypes as GHC
import DynFlags as GHC
import FamInstEnv as GHC
import qualified GHC
import Id as GHC
import Var
import InstEnv as GHC
import Module as GHC
import Name as GHC
import RdrName as GHC
import SrcLoc as GHC
import Type as GHC
import HscTypes as GHC
import CoAxiom as GHC
import HsExtension (IdP)

import Data.Data as Data
import Control.Reference
import Control.Monad.IO.Class

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
                           , _nameInfo :: IdP n
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

deriving instance (Data n, Typeable n, Data (IdP n)) => Data (NameInfo n)
-- deriving instance Functor NameInfo
-- deriving instance Foldable NameInfo
-- deriving instance Traversable NameInfo

-- | Creates semantic information for an unambiguous name
mkNameInfo :: Scope -> Bool -> IdP n -> NameInfo n
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
  = PName { _pName :: IdP n
          , _pNameParent :: Maybe (IdP n)
          }

deriving instance (Data n, Typeable n, Data (IdP n)) => Data (PName n)

trfPNames :: (IdP n -> IdP n') -> PName n -> PName n'
trfPNames f (PName name parent) = PName (f name) (fmap f parent)

trfPNamesM :: Monad m => (IdP n -> m (IdP n')) -> PName n -> m (PName n')
trfPNamesM f (PName name (Just parent)) = PName <$> f name <*> (Just <$> f parent)
trfPNamesM f (PName name Nothing) = PName <$> f name <*> return Nothing

-- | Info for the module element
data ModuleInfo n = ModuleInfo { _defModuleName :: GHC.Module
                               , _defDynFlags :: DynFlags -- ^ The compilation flags that are set up when the module was compiled
                               , _defIsBootModule :: Bool -- ^ True if this module is created from a hs-boot file
                               , _implicitNames :: [PName n] -- ^ implicitly imported names
                               , _prelTransMods :: [GHC.Module] -- ^ Modules imported transitively.
                               }

trfModuleInfoM :: Monad m => (IdP n -> m (IdP n')) -> ModuleInfo n -> m (ModuleInfo n')
trfModuleInfoM f (ModuleInfo mn df bm impl tm)
  = ModuleInfo mn df bm <$> mapM (trfPNamesM f) impl <*> return tm

deriving instance (Data n, Typeable n, Data (IdP n)) => Data (ModuleInfo n)

instance Data DynFlags where
  gunfold _ _ _ = error "Cannot construct dyn flags"
  toConstr _ = dynFlagsCon
  dataTypeOf _ = dynFlagsType

dynFlagsType = mkDataType "DynFlags.DynFlags" [dynFlagsCon]
dynFlagsCon = mkConstr dynFlagsType "DynFlags" [] Data.Prefix

-- | Creates semantic information for the module element.
-- Strict in the list of implicitely imported, orphan and family instances.
mkModuleInfo :: GHC.Module -> DynFlags -> Bool -> [PName n] -> [GHC.Module] -> ModuleInfo n
-- the calculate of these fields involves a big parts of the GHC state and it causes a space leak
-- if not evaluated strictly
mkModuleInfo mod dfs boot !imported deps = ModuleInfo mod dfs boot imported deps

-- | Info corresponding to an import declaration
data ImportInfo n = ImportInfo { _importedModule :: GHC.Module -- ^ The name and package of the imported module
                               , _availableNames :: [IdP n] -- ^ Names available from the imported module
                               , _importedNames :: [PName n] -- ^ Names actually imported from the module.
                               , _importTransMods :: [GHC.Module] -- ^ Modules imported transitively.
                               }

trfImportInfo :: (IdP n -> IdP n') -> ImportInfo n -> ImportInfo n'
trfImportInfo f (ImportInfo mod avail imp trm)
  = ImportInfo mod (map f avail) (map (trfPNames f) imp) trm

trfImportInfoM :: Monad m => (IdP n -> m (IdP n')) -> ImportInfo n -> m (ImportInfo n')
trfImportInfoM f (ImportInfo mod avail imp trm)
  = ImportInfo mod <$> (mapM f avail) <*> (mapM (trfPNamesM f) imp) <*> return trm

deriving instance (Data n, Typeable n, Data (IdP n)) => Data (ImportInfo n)

deriving instance Data FamInst
deriving instance Data FamFlavor

-- | Creates semantic information for an import declaration
-- Strict in the list of the used and imported declarations, orphan and family instances.
mkImportInfo :: GHC.Module -> [IdP n] -> [PName n] -> [GHC.Module] -> ImportInfo n
-- the calculate of these fields involves a big parts of the GHC state and it causes a space leak
-- if not evaluated strictly
mkImportInfo mod !names !imported deps = ImportInfo mod names imported deps

-- | Gets the class and family instances from a module.
getInstances :: [Module] -> GHC.Ghc ([ClsInst], [FamInst])
getInstances mods = do
  env <- GHC.getSession
  eps <- liftIO $ hscEPS env
  let (hptInsts, hptFamInsts) = hptInstances env (`elem` map GHC.moduleName mods)
      isFromMods inst = maybe False (`elem` mods) $ nameModule_maybe $ Var.varName $ is_dfun inst
      famIsFromMods inst = maybe False (`elem` mods) $ nameModule_maybe $ co_ax_name $ fi_axiom inst
      epsInsts = filter isFromMods $ instEnvElts $ eps_inst_env eps
      epsFamInsts = filter famIsFromMods $ famInstEnvElts $ eps_fam_inst_env eps
  return (hptInsts ++ epsInsts, hptFamInsts ++ epsFamInsts)

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
