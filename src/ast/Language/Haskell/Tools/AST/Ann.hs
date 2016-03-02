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

data SpanInfo 
  = NodeSpan SrcSpan
  | ListPos SrcLoc
  | OptionalPos SrcLoc

spanRange :: SpanInfo -> SrcSpan
spanRange (NodeSpan sp) = sp
spanRange (ListPos pos) = srcLocSpan pos
spanRange (OptionalPos pos) = srcLocSpan pos
  
type RangeInfo = NodeInfo () SpanInfo
type RangeWithName = NodeInfo SemanticInfo SpanInfo

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
data AnnList e a = AnnList { _annListPos :: a 
                           , _annList :: [Ann e a]
                           }

makeLenses ''AnnList

-- | An optional AST element
data AnnMaybe e a = AnnMaybe { _annMaybePos :: a 
                             , _annMaybe :: (Maybe (Ann e a))
                             }

makeLenses ''AnnMaybe

-- | An empty list of AST elements
annNil :: a -> AnnList e a
annNil a = AnnList a []

isAnnNothing :: AnnMaybe e a -> Bool
isAnnNothing (AnnMaybe _ Nothing) = True
isAnnNothing (AnnMaybe _ _) = False

-- | An existing AST element
annJust :: Ann e a -> AnnMaybe e a
annJust e = AnnMaybe (e ^. annotation) (Just e)

-- | A non-existing AST part
annNothing :: a -> AnnMaybe e a
annNothing a = AnnMaybe a Nothing
