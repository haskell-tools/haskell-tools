-- | Utility methods for generating parts of the ADT for refactorings
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.Tools.AST.Gen.Utils where

import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

emptyList :: TemplateAnnot a => AnnList e a
emptyList = AnnList (fromTemplate list) []
              
replaceWithJust :: TemplateAnnot a => Ann e a -> AnnMaybe e a -> AnnMaybe e a            
replaceWithJust e (AnnMaybe temp _) = AnnMaybe temp (Just e)

justVal :: TemplateAnnot a => Ann e a -> AnnMaybe e a
justVal e = AnnMaybe (fromTemplate opt) (Just e)

noth :: TemplateAnnot a => AnnMaybe e a
noth = AnnMaybe (fromTemplate opt) Nothing

mkAnn :: TemplateAnnot a => SourceTemplate -> e a -> Ann e a
mkAnn temp = Ann (fromTemplate temp)

-- | Annotation for a simple wrapper AST node
wrapperAnn :: TemplateAnnot a => e a -> Ann e a
wrapperAnn = mkAnn child

-- | Transforms the list of elements to an AnnList with the given source template.
mkAnnList :: TemplateAnnot a => SourceTemplate -> [Ann e a] -> AnnList e a
mkAnnList temp = AnnList (fromTemplate temp)

-- | Transforms the Maybe element to an AnnMaybe with the given source template.
mkAnnMaybe :: TemplateAnnot a => SourceTemplate -> Maybe (Ann e a) -> AnnMaybe e a
mkAnnMaybe temp = AnnMaybe (fromTemplate temp)
  
instance TemplateAnnot (NodeInfo (SemanticInfo n) SourceTemplate) where
  fromTemplate = NodeInfo NoSemanticInfo
  getTemplate = (^. sourceInfo)
  
instance TemplateAnnot (NodeInfo () SourceTemplate) where
  fromTemplate = NodeInfo ()
  getTemplate = (^. sourceInfo)