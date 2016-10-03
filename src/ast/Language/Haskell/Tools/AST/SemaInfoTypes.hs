{-# LANGUAGE DeriveDataTypeable
           , TemplateHaskell 
           , UndecidableInstances
           , FlexibleContexts
           , FlexibleInstances
           #-}
module Language.Haskell.Tools.AST.SemaInfoTypes where

import Name as GHC
import BasicTypes as GHC
import Id as GHC
import Module as GHC
import SrcLoc as GHC
import RdrName as GHC
import Outputable as GHC

import Data.List
import Data.Data

import Control.Reference

type Scope = [[Name]]

-- | Semantic info type for any node not 
-- carrying additional semantic information
data NoSemanticInfo = NoSemanticInfo 
  deriving (Eq, Data)

-- | Info for expressions that tells which definitions are in scope
data ScopeInfo = ScopeInfo { _exprScopedLocals :: Scope 
                           }
  deriving (Eq, Data)

-- | Info corresponding to a name
data NameInfo n = NameInfo { _nameScopedLocals :: Scope
                           , _nameIsDefined :: Bool
                           , _nameInfo :: n
                           } 
                | AmbiguousNameInfo { _nameScopedLocals :: Scope
                                    , _nameIsDefined :: Bool
                                    , _ambiguousName :: RdrName
                                    , _ambiguousLocation :: SrcSpan
                                    }
                | ImplicitNameInfo { _nameScopedLocals :: Scope
                                   , _nameIsDefined :: Bool
                                   , _implicitName :: String
                                   , _implicitLocation :: SrcSpan
                                   }

  deriving (Eq, Data)

-- | Info corresponding to a name that is correctly identified
data CNameInfo = CNameInfo { _cnameScopedLocals :: Scope
                           , _cnameIsDefined :: Bool
                           , _cnameInfo :: Id
                           , _cnameFixity :: Maybe GHC.Fixity
                           }
  deriving (Eq, Data)

-- | Info for the module element
data ModuleInfo n = ModuleInfo { _defModuleName :: GHC.Module 
                               , _defIsBootModule :: Bool -- ^ True if this module is created from a hs-boot file
                               , _implicitNames :: [n] -- ^ Implicitely imported names
                               } 
  deriving (Eq, Data)

-- | Info corresponding to an import declaration
data ImportInfo n = ImportInfo { _importedModule :: GHC.Module -- ^ The name and package of the imported module
                               , _availableNames :: [n] -- ^ Names available from the imported module
                               , _importedNames :: [n] -- ^ Names actually imported from the module.
                               } 
  deriving (Eq, Data)

-- | Info corresponding to an record-wildcard
data ImplicitFieldInfo = ImplicitFieldInfo { _implicitFieldBindings :: [(Name, Name)] -- ^ The implicitely bounded names
                                           } 
  deriving (Eq, Data)

instance Show ScopeInfo where
  show (ScopeInfo locals) = "(ScopeInfo " ++ showSDocUnsafe (ppr locals) ++ ")"

instance Outputable n => Show (NameInfo n) where
  show (NameInfo locals defined nameInfo) = "(NameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ ")"
  show (AmbiguousNameInfo locals defined nameInfo span) = "(AmbiguousNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ " " ++ show span ++ ")"
  show (ImplicitNameInfo locals defined nameInfo span) = "(ImplicitNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ " " ++ show span ++ ")"

instance Show CNameInfo where
  show (CNameInfo locals defined nameInfo fixity) = "(CNameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ showSDocUnsafe (ppr fixity) ++ ")"

instance Outputable n => Show (ModuleInfo n) where
  show (ModuleInfo mod isboot imp) = "(ModuleInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ show isboot ++ " " ++ showSDocUnsafe (ppr imp) ++ ")"

instance Outputable n => Show (ImportInfo n) where
  show (ImportInfo mod avail imported) = "(ImportInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ showSDocUnsafe (ppr avail) ++ " " ++ showSDocUnsafe (ppr imported) ++ ")"

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

-- | Semantic and source code related information for an AST node.
data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }
  deriving (Eq, Show, Data)
             
makeReferences ''NodeInfo