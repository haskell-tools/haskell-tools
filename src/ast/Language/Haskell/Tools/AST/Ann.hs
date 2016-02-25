{-# LANGUAGE FlexibleInstances
           , TemplateHaskell
           , DeriveDataTypeable
           #-}

-- | Parts of AST representation for keeping extra data
module Language.Haskell.Tools.AST.Ann where

import Data.Data
import Control.Lens
import SrcLoc
import Name
import Module
import Id

-- | An element of the AST keeping extra information.
data Ann elem annot
-- The type parameters are organized this way because we want the annotation type to
-- be more flexible, but the annotation is the first parameter because it eases 
-- pattern matching.
  = Ann { _annotation :: annot -- ^ The extra information for the AST part
        , _element    :: elem annot -- ^ The original AST part
        }
        
makeLenses ''Ann
        
-- | Semantic and source code related information for an AST node.
data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }
  deriving (Eq, Show, Data)
             
makeLenses ''NodeInfo

type RangeInfo = NodeInfo () SrcSpan
type RangeWithName = NodeInfo SemanticInfo SrcSpan

-- | Semantic information for an AST node. Semantic information is
-- currently heterogeneous.
data SemanticInfo 
  = NoSemanticInfo -- ^ Semantic info type for any node not 
                   -- carrying additional semantic information
  | NameInfo { _nameInfo :: Name 
             } -- ^ Info corresponding to a name
  | ImportInfo { _importedModule :: Module -- ^ The name and package of the imported module
               , _availableNames :: [Name] -- ^ Names available from the imported module
               , _importedNames :: [Name] -- ^ Names actually imported from the module.
               } -- ^ Info corresponding to an import declaration
  -- | ImplicitImports [ImportDecl]
  -- | ImplicitFieldUpdates [ImportDecl]
  deriving (Eq, Data)
    
makeLenses ''SemanticInfo

-- | A list of AST elements
newtype AnnList e a = AnnList { _annList :: [Ann e a] }

makeLenses ''AnnList

-- | An optional AST element
newtype AnnMaybe e a = AnnMaybe { _annMaybe :: (Maybe (Ann e a)) }

makeLenses ''AnnMaybe

-- | An empty list of AST elements
annNil :: AnnList e a
annNil = AnnList []

isAnnNothing :: AnnMaybe e a -> Bool
isAnnNothing (AnnMaybe Nothing) = True
isAnnNothing (AnnMaybe _) = False

-- | An existing AST element
annJust :: Ann e a -> AnnMaybe e a
annJust = AnnMaybe . Just

-- | A non-existing AST part
annNothing :: AnnMaybe e a
annNothing = AnnMaybe Nothing
