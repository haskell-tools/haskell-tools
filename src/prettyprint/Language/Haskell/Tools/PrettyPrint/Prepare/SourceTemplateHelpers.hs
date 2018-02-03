{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}

-- | Helper functions for working with source templates
module Language.Haskell.Tools.PrettyPrint.Prepare.SourceTemplateHelpers where

import Control.Reference ((.=))
import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare.SourceTemplate
import SrcLoc (noSrcSpan, combineSrcSpans)

type ASTElement node dom = Ann node dom SrcTemplateStage
type ASTOptional node dom = AnnMaybeG node dom SrcTemplateStage
type ASTMulti node dom = AnnListG node dom SrcTemplateStage

instance IsString (SpanInfo SrcTemplateStage) where
  fromString s = SourceTemplateNode noSrcSpan [TextElem [NormalText s] noSrcSpan] 0 Nothing

-- * Basic elements
child :: SpanInfo SrcTemplateStage
child = SourceTemplateNode noSrcSpan [ChildElem] 0 Nothing

opt :: OptionalInfo SrcTemplateStage
opt = SourceTemplateOpt noSrcSpan "" "" 0 Nothing

list :: ListInfo SrcTemplateStage
list = SourceTemplateList noSrcSpan "" "" "" Nothing [] 0 Nothing

-- * Modifiers

class AfterBefore i where
  -- | Put the given string before the element if it is not empty
  after :: String -> i -> i
  -- | The given string should follow the element if it is not empty
  followedBy :: String -> i -> i

instance AfterBefore (ListInfo SrcTemplateStage) where
  after str = srcTmpListBefore .= str
  followedBy str = srcTmpListAfter .= str

instance AfterBefore (OptionalInfo SrcTemplateStage) where
  after str = srcTmpOptBefore .= str
  followedBy str = srcTmpOptAfter .= str

class RelativeIndent i where
  -- | The element should be indented relatively to its parent
  relativeIndented :: Int -> i -> i

instance RelativeIndent (SpanInfo SrcTemplateStage) where
  relativeIndented i = srcTmpRelPos .= Just i

instance RelativeIndent (ListInfo SrcTemplateStage) where
  relativeIndented i = srcTmpListRelPos .= Just i

instance RelativeIndent (OptionalInfo SrcTemplateStage) where
  relativeIndented i = srcTmpOptRelPos .= Just i


class MinimumIndent i where
  -- | The elements should be indented at least to the given number of spaces
  minimumIndented :: Int -> i -> i

instance MinimumIndent (SpanInfo SrcTemplateStage) where
  minimumIndented i = sourceTemplateMinimalIndent .= i

instance MinimumIndent (ListInfo SrcTemplateStage) where
  minimumIndented i = srcTmpListMinimalIndent .= i

instance MinimumIndent (OptionalInfo SrcTemplateStage) where
  minimumIndented i = srcTmpOptMinimalIndent .= i

-- | The elements of the list should be separated by the given string by default (might be overridden)
separatedBy :: String -> ListInfo SrcTemplateStage -> ListInfo SrcTemplateStage
separatedBy sep = srcTmpDefaultSeparator .= sep

-- | The elements of the list should be indented on the same column
indented :: ListInfo SrcTemplateStage -> ListInfo SrcTemplateStage
indented = (srcTmpIndented .= Just []) . (srcTmpDefaultSeparator .= "\n")

-- | Concatenates two source templates to produce a new template with all child elements.
(<>) :: SpanInfo SrcTemplateStage -> SpanInfo SrcTemplateStage -> SpanInfo SrcTemplateStage
SourceTemplateNode sp1 el1 _ _ <> SourceTemplateNode sp2 el2 _ _ = SourceTemplateNode (combineSrcSpans sp1 sp2) (el1 ++ el2) 0 Nothing
