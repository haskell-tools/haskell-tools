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
import Id

-- | An element of the AST keeping extra information.
data Ann elem annot
-- The type parameters are organized this way because we want the annotation type to
-- be more flexible, but the annotation is the first parameter because it eases 
-- pattern matching.
  = Ann { _annotation :: annot -- ^ The extra information for the AST part
        , _element    :: elem annot -- ^ The original AST part
        }
        
data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }
  deriving (Show, Data)
             
makeLenses ''NodeInfo

data SemanticInfo 
  = NameInfo Name
  -- | ImplicitImports [ImportDecl]
  -- | ImplicitFieldUpdates [ImportDecl]

type RangeInfo = NodeInfo () SrcSpan
type RangeWithName = NodeInfo (Maybe SemanticInfo) SrcSpan

class RangeAnnot annot where
  toRangeAnnot :: SrcSpan -> annot
  extractRange :: annot -> SrcSpan
  
instance RangeAnnot (NodeInfo (Maybe a) SrcSpan) where
  toRangeAnnot = NodeInfo Nothing
  extractRange = view sourceInfo 
  
instance RangeAnnot (NodeInfo () SrcSpan) where
  toRangeAnnot = NodeInfo ()
  extractRange = view sourceInfo



-- | A list of AST elements
newtype AnnList e a = AnnList { _fromAnnList :: [Ann e a] }

-- | An optional AST element
newtype AnnMaybe e a = AnnMaybe { _fromAnnMaybe :: (Maybe (Ann e a)) }

-- | An empty list of AST elements
annNil :: AnnList e a
annNil = AnnList []

-- | An existing AST element
annJust :: Ann e a -> AnnMaybe e a
annJust = AnnMaybe . Just

-- | A non-existing AST part
annNothing :: AnnMaybe e a
annNothing = AnnMaybe Nothing
