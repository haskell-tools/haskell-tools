{-# LANGUAGE DeriveDataTypeable
           , TemplateHaskell 
           , UndecidableInstances
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.AST.SemaInfoTypes where

import Name
import Id
import Module
import SrcLoc
import RdrName
import Outputable

import Data.Data

import Control.Reference

-- | Semantic info type for any node not 
-- carrying additional semantic information
data NoSemanticInfo = NoSemanticInfo 
  deriving (Eq, Data)

-- | Info for expressions that tells which definitions are in scope
data ScopeInfo = ScopeInfo { _exprScopedLocals :: [[Name]] 
                           }
  deriving (Eq, Data)

-- | Info corresponding to a name
data NameInfo n = NameInfo { _nameScopedLocals :: [[Name]]
                           , _nameIsDefined :: Bool
                           , _nameInfo :: n
                           } 
                | AmbiguousNameInfo { _nameScopedLocals :: [[Name]]
                                    , _nameIsDefined :: Bool
                                    , _ambiguousName :: RdrName
                                    , _ambiguousLocation :: SrcSpan
                                    }
                | ImplicitNameInfo { _nameScopedLocals :: [[Name]]
                                   , _nameIsDefined :: Bool
                                   , _implicitName :: String
                                   }

  deriving (Eq, Data)

-- | Info corresponding to a name that is correctly identified
data CNameInfo = CNameInfo { _cnameScopedLocals :: [[Name]]
                           , _cnameIsDefined :: Bool
                           , _cnameInfo :: Id
                           }
  deriving (Eq, Data)

-- | Info for the module element
data ModuleInfo n = ModuleInfo { _defModuleName :: Module 
                               , _implicitNames :: [n] -- ^ Implicitely imported names
                               } 
  deriving (Eq, Data)

-- | Info corresponding to an import declaration
data ImportInfo n = ImportInfo { _importedModule :: Module -- ^ The name and package of the imported module
                               , _availableNames :: [n] -- ^ Names available from the imported module
                               , _importedNames :: [n] -- ^ Names actually imported from the module.
                               } 
  deriving (Eq, Data)

instance Show NoSemanticInfo where
  show NoSemanticInfo = "NoSemanticInfo"

instance Show ScopeInfo where
  show (ScopeInfo locals) = "(ScopeInfo " ++ showSDocUnsafe (ppr locals) ++ ")"

instance Outputable n => Show (NameInfo n) where
  show (NameInfo locals defined nameInfo) = "(NameInfo " ++ showSDocUnsafe (ppr locals) ++ " " ++ show defined ++ " " ++ showSDocUnsafe (ppr nameInfo) ++ ")"

instance Outputable n => Show (ModuleInfo n) where
  show (ModuleInfo mod imp) = "(ModuleInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ showSDocUnsafe (ppr imp) ++ ")"

instance Outputable n => Show (ImportInfo n) where
  show (ImportInfo mod avail imported) = "(ImportInfo " ++ showSDocUnsafe (ppr mod) ++ " " ++ showSDocUnsafe (ppr avail) ++ " " ++ showSDocUnsafe (ppr imported) ++ ")"

makeReferences ''NoSemanticInfo
makeReferences ''ScopeInfo
makeReferences ''NameInfo
makeReferences ''ModuleInfo
makeReferences ''ImportInfo


-- | Semantic and source code related information for an AST node.
data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }
  deriving (Eq, Show, Data)
             
makeReferences ''NodeInfo