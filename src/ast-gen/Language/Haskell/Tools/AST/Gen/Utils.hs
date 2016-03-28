-- | Utility methods for generating parts of the ADT for refactorings
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.Tools.AST.Gen.Utils where

import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

emptyList :: TemplateAnnot a => AnnList e a
emptyList = AnnList (fromTemplate list) []
              
toJust :: TemplateAnnot a => Ann e a -> AnnMaybe e a -> AnnMaybe e a            
toJust e (AnnMaybe temp _) = AnnMaybe temp (Just e)

noth :: TemplateAnnot a => AnnMaybe e a
noth = AnnMaybe (fromTemplate opt) Nothing

mkAnn :: TemplateAnnot a => SourceTemplate -> e a -> Ann e a
mkAnn temp e = Ann (fromTemplate temp) e

wrapperAnn :: TemplateAnnot a => e a -> Ann e a
wrapperAnn = mkAnn child

mkAnnList :: TemplateAnnot a => SourceTemplate -> [Ann e a] -> AnnList e a
mkAnnList temp ls = AnnList (fromTemplate temp) ls

mkAnnMaybe :: TemplateAnnot a => SourceTemplate -> Maybe (Ann e a) -> AnnMaybe e a
mkAnnMaybe temp mb = AnnMaybe (fromTemplate temp) mb
  
instance TemplateAnnot (NodeInfo (SemanticInfo n) SourceTemplate) where
  fromTemplate = NodeInfo NoSemanticInfo
  getTemplate = (^. sourceInfo)
  
instance TemplateAnnot (NodeInfo () SourceTemplate) where
  fromTemplate = NodeInfo ()
  getTemplate = (^. sourceInfo)