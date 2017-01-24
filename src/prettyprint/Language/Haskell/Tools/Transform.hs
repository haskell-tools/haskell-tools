-- | A module for preparing the representation of the AST for pretty printing.
module Language.Haskell.Tools.Transform
  ( prepareAST
  -- comment handling
  , placeComments, getNormalComments, getPragmaComments
  -- generating source templates
  , child, opt, list, after, followedBy, relativeIndented, minimumIndented, separatedBy, indented, (<>)
  -- references on source templates
  , sourceTemplateNodeRange, sourceTemplateNodeElems
  , sourceTemplateListRange, srcTmpListBefore, srcTmpListAfter, srcTmpDefaultSeparator, srcTmpIndented, srcTmpSeparators
  , sourceTemplateOptRange, srcTmpOptBefore, srcTmpOptAfter
  -- parts of the transformation, used for debugging purposes
  , rangeToSource, fixRanges, cutUpRanges, getLocIndices, mapLocIndices
  ) where

import Language.Haskell.Tools.Transform.PlaceComments (getNormalComments, getPragmaComments, placeComments)
import Language.Haskell.Tools.Transform.RangeTemplate ()
import Language.Haskell.Tools.Transform.RangeTemplateToSourceTemplate (rangeToSource, getLocIndices, mapLocIndices)
import Language.Haskell.Tools.Transform.RangeToRangeTemplate (cutUpRanges, fixRanges)
import Language.Haskell.Tools.Transform.SourceTemplate
import Language.Haskell.Tools.Transform.SourceTemplateHelpers

import Language.Haskell.Tools.AST
import StringBuffer (StringBuffer)

-- | Prepares the AST for pretty printing
prepareAST :: SourceInfoTraversal node => StringBuffer -> Ann node dom RangeStage -> Ann node dom SrcTemplateStage
prepareAST srcBuffer = rangeToSource srcBuffer . cutUpRanges . fixRanges
