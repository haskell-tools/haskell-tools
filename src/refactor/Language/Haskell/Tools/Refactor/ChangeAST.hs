{-# LANGUAGE LambdaCase, TupleSections, AllowAmbiguousTypes #-}
-- | Operations for changing the AST
module Language.Haskell.Tools.Refactor.ChangeAST (removeChild, removeSeparator) where

import Control.Reference
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform
import Language.Haskell.Tools.Refactor.RefactorBase
import SrcLoc

import Debug.Trace

-- | Remove a separator from the AST while keeping the textual parts of it that should not be removed (like preprocessor pragmas).
removeSeparator :: ([SourceTemplateTextElem], SrcSpan) -> LocalRefactor dom ()
removeSeparator (txts, range) = tell staying
  where staying = mapMaybe (\case StayingText str lnEnd -> Just (Right (range, str, lnEnd))
                                  _ -> Nothing) txts

-- | Remove an element from the AST while keeping the textual parts of it that should not be removed (like preprocessor pragmas).
removeChild :: (SourceInfoTraversal e) => e dom SrcTemplateStage -> LocalRefactor dom ()
removeChild e = tell $ map Right $ keptText e

-- | Extracts all text elements that should be kept
keptText :: SourceInfoTraversal e => e dom SrcTemplateStage -> [(SrcSpan,String,String)]
keptText = execWriter . sourceInfoTraverse (SourceInfoTrf
  (\ni -> mapM_ writeStaying (mapMaybe (\case (TextElem elems range) -> Just (elems,range)
                                              _ -> Nothing)
                                       (ni ^? sourceTemplateNodeElems & traversal)) >> return ni)
  (\ni -> mapM_ writeStaying (ni ^. srcTmpSeparators) >> return ni)
  pure)

-- | Writes the elements to be kept into a writer monad with ranges and default line ending.
writeStaying :: ([SourceTemplateTextElem], SrcSpan) -> Writer [(SrcSpan,String,String)] ()
writeStaying (txts, range) = tell staying
  where staying = mapMaybe (\case StayingText str lnEnd -> Just (range, str, lnEnd)
                                  _ -> Nothing) txts
