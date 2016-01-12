-- | Parts of AST representation for keeping extra data
module Language.Haskell.Tools.AST.Ann where

-- | An element of the AST keeping extra information.
data Ann elem annot
-- The type parameters are organized this way because we want the annotation type to
-- be more flexible, but the annotation is the first parameter because it eases 
-- pattern matching.
  = Ann { annotation :: annot -- ^ The extra information for the AST part
        , element    :: elem annot -- ^ The original AST part
        }

-- | A list of AST elements
newtype AnnList e a = AnnList { fromAnnList :: [Ann e a] }

-- | An optional AST element
newtype AnnMaybe e a = AnnMaybe { fromAnnMaybe :: (Maybe (Ann e a)) }

-- | An empty list of AST elements
annNil :: AnnList e a
annNil = AnnList []

-- | An existing AST element
annJust :: Ann e a -> AnnMaybe e a
annJust = AnnMaybe . Just

-- | A non-existing AST part
annNothing :: AnnMaybe e a
annNothing = AnnMaybe Nothing
