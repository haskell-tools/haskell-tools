-- | A module for preparing the representation of the AST for pretty printing.
module Language.Haskell.Tools.PrettyPrint.Prepare
  ( prepareAST, prepareASTCpp
  -- comment handling
  , placeComments, getNormalComments, getPragmaComments
  -- generating source templates
  , child, opt, list, after, followedBy, relativeIndented, minimumIndented, separatedBy, indented
  -- references on source templates
  , sourceTemplateNodeRange, sourceTemplateNodeElems
  , sourceTemplateListRange, srcTmpListBefore, srcTmpListAfter, srcTmpDefaultSeparator, srcTmpIndented, srcTmpSeparators
  , sourceTemplateOptRange, srcTmpOptBefore, srcTmpOptAfter
  , SourceTemplateElem(..), sourceTemplateTextElem, sourceTemplateTextRange, SourceTemplateTextElem(..), sourceTemplateText, lineEndings, isStayingText
  -- parts of the transformation, used for debugging purposes
  , rangeToSource, fixRanges, cutUpRanges, getLocIndices, mapLocIndices, fixMainRange, extractStayingElems
  , sourceTemplateMinimalIndent, srcTmpListMinimalIndent, srcTmpOptMinimalIndent
  , TransformationProblem(..), BreakUpProblem(..)
  ) where

import Language.Haskell.Tools.PrettyPrint.Prepare.PlaceComments (getNormalComments, getPragmaComments, placeComments)
import Language.Haskell.Tools.PrettyPrint.Prepare.RangeTemplate (TransformationProblem(..))
import Language.Haskell.Tools.PrettyPrint.Prepare.RangeTemplateToSourceTemplate (rangeToSource, getLocIndices, mapLocIndices, extractStayingElems)
import Language.Haskell.Tools.PrettyPrint.Prepare.RangeToRangeTemplate (cutUpRanges, fixRanges, BreakUpProblem(..))
import Language.Haskell.Tools.PrettyPrint.Prepare.SourceTemplate
import Language.Haskell.Tools.PrettyPrint.Prepare.SourceTemplateHelpers

import FastString (mkFastString)
import Language.Haskell.Tools.AST
import SrcLoc
import StringBuffer (StringBuffer, nextChar, atEnd)

-- | Prepares the AST for pretty printing
prepareAST :: StringBuffer -> Ann UModule dom RangeStage -> Ann UModule dom SrcTemplateStage
prepareAST srcBuffer = rangeToSource srcBuffer . cutUpRanges . fixRanges

prepareASTCpp :: StringBuffer -> Ann UModule dom RangeStage -> Ann UModule dom SrcTemplateStage
prepareASTCpp srcBuffer = extractStayingElems . rangeToSource srcBuffer . cutUpRanges . fixRanges . fixMainRange srcBuffer

fixMainRange :: StringBuffer -> Ann UModule dom RangeStage -> Ann UModule dom RangeStage
fixMainRange buffer mod = setRange (mkSrcSpan (srcSpanStart $ getRange mod) (RealSrcLoc (endPos startPos buffer))) mod
  where startPos = mkRealSrcLoc (mkFastString "") 1 1
        endPos pos buf | atEnd buf = pos
        endPos pos buf = let (ch,buf') = nextChar buf in endPos (advanceSrcLoc pos ch) buf'
