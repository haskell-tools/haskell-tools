-- | Utility methods for generating parts of the ADT for refactorings
{-# LANGUAGE FlexibleInstances
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Utils where

import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

fromTemplate :: src -> NodeInfo sema src
fromTemplate = NodeInfo (error "The newly generated AST fragments have no semantic info")

emptyList :: AnnList e dom SrcTemplateStage
emptyList = AnnList (fromTemplate list) []
              
replaceWithJust :: Ann e dom SrcTemplateStage -> AnnMaybe e dom SrcTemplateStage -> AnnMaybe e dom SrcTemplateStage           
replaceWithJust e (AnnMaybe temp _) = AnnMaybe temp (Just e)

justVal :: Ann e dom SrcTemplateStage -> AnnMaybe e dom SrcTemplateStage
justVal e = AnnMaybe (fromTemplate opt) (Just e)

noth :: AnnMaybe e dom SrcTemplateStage
noth = AnnMaybe (fromTemplate opt) Nothing

mkAnn :: SpanInfo SrcTemplateStage -> e dom SrcTemplateStage -> Ann e dom SrcTemplateStage
mkAnn temp = Ann (fromTemplate temp)

-- | Annotation for a simple wrapper AST node
wrapperAnn :: e dom SrcTemplateStage -> Ann e dom SrcTemplateStage
wrapperAnn = mkAnn child

-- | Transforms the list of elements to an AnnList with the given source template.
mkAnnList :: ListInfo SrcTemplateStage -> [Ann e dom SrcTemplateStage] -> AnnList e dom SrcTemplateStage
mkAnnList temp = AnnList (fromTemplate temp)

-- | Transforms the Maybe element to an AnnMaybe with the given source template.
mkAnnMaybe :: OptionalInfo SrcTemplateStage -> Maybe (Ann e dom SrcTemplateStage) -> AnnMaybe e dom SrcTemplateStage
mkAnnMaybe temp = AnnMaybe (fromTemplate temp)
