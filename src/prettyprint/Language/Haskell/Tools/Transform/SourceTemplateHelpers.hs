{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , FlexibleInstances
           #-}
-- | Helper functions for working with source templates
module Language.Haskell.Tools.Transform.SourceTemplateHelpers where

import SrcLoc
import Data.String
import Data.List
import Control.Reference
import Data.Function (on)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.SourceTemplate

type ASTElement node dom = Ann node dom SrcTemplateStage
type ASTOptional node dom = AnnMaybeG node dom SrcTemplateStage
type ASTMulti node dom = AnnListG node dom SrcTemplateStage

instance IsString (SpanInfo SrcTemplateStage) where
  fromString s = SourceTemplateNode noSrcSpan [TextElem s]
    
child :: SpanInfo SrcTemplateStage
child = SourceTemplateNode noSrcSpan [ChildElem]

opt :: OptionalInfo SrcTemplateStage
opt = SourceTemplateOpt noSrcSpan "" ""

optBefore :: String -> OptionalInfo SrcTemplateStage
optBefore s = SourceTemplateOpt noSrcSpan s ""

optAfter :: String -> OptionalInfo SrcTemplateStage
optAfter s = SourceTemplateOpt noSrcSpan "" s

optBeforeAfter :: String -> String -> OptionalInfo SrcTemplateStage
optBeforeAfter bef aft = SourceTemplateOpt noSrcSpan bef aft

list :: ListInfo SrcTemplateStage
list = SourceTemplateList noSrcSpan "" "" "" False []

indentedList :: ListInfo SrcTemplateStage
indentedList = SourceTemplateList noSrcSpan "" "" "\n" True []

indentedListBefore :: String -> ListInfo SrcTemplateStage
indentedListBefore bef = SourceTemplateList noSrcSpan bef "" "\n" True []

indentedListAfter :: String -> ListInfo SrcTemplateStage
indentedListAfter aft = SourceTemplateList noSrcSpan "" aft "\n" True []

listSep :: String -> ListInfo SrcTemplateStage
listSep s = SourceTemplateList noSrcSpan "" "" s False []

listSepBefore :: String -> String -> ListInfo SrcTemplateStage
listSepBefore s bef = SourceTemplateList noSrcSpan bef "" s False []

listSepAfter :: String -> String -> ListInfo SrcTemplateStage
listSepAfter s aft = SourceTemplateList noSrcSpan "" aft s False []

listSepBeforeAfter :: String -> String -> String -> ListInfo SrcTemplateStage
listSepBeforeAfter s bef aft = SourceTemplateList noSrcSpan bef aft s False []

-- | Concatenates two source templates to produce a new template with all child elements.
(<>) :: SpanInfo SrcTemplateStage -> SpanInfo SrcTemplateStage -> SpanInfo SrcTemplateStage
SourceTemplateNode sp1 el1 <> SourceTemplateNode sp2 el2 = SourceTemplateNode (combineSrcSpans sp1 sp2) (el1 ++ el2)
