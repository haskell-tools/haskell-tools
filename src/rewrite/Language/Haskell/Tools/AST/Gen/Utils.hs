-- | Utility methods for generating parts of the AST for refactorings
{-# LANGUAGE FlexibleInstances
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Utils where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes (AnnMaybe, AnnList)
import Language.Haskell.Tools.Transform (child, opt, list)

fromTemplate :: src -> NodeInfo sema src
fromTemplate = NodeInfo (error "The newly generated AST fragments have no semantic info")

emptyList :: AnnList e dom
emptyList = AnnListG (fromTemplate list) []

justVal :: Ann e dom SrcTemplateStage -> AnnMaybeG e dom SrcTemplateStage
justVal e = AnnMaybeG (fromTemplate opt) (Just e)

noth :: AnnMaybe e dom
noth = AnnMaybeG (fromTemplate opt) Nothing

mkAnn :: SpanInfo SrcTemplateStage -> e dom SrcTemplateStage -> Ann e dom SrcTemplateStage
mkAnn temp = Ann (fromTemplate temp)

-- | Annotation for a simple wrapper AST node
wrapperAnn :: e dom SrcTemplateStage -> Ann e dom SrcTemplateStage
wrapperAnn = mkAnn child

-- | Transforms the list of elements to an AnnListG with the given source template.
mkAnnList :: ListInfo SrcTemplateStage -> [Ann e dom SrcTemplateStage] -> AnnList e dom
mkAnnList temp = AnnListG (fromTemplate temp)

-- | Transforms the Maybe element to an AnnMaybeG with the given source template.
mkAnnMaybe :: OptionalInfo SrcTemplateStage -> Maybe (Ann e dom SrcTemplateStage) -> AnnMaybe e dom
mkAnnMaybe temp = AnnMaybeG (fromTemplate temp)
