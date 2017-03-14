{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , LambdaCase
           #-}
-- | This transformation expands nodes to contain the comments that should be attached to them. After this, a
-- normalizing transformation should be performed that expands parents to contain their children.
module Language.Haskell.Tools.Transform.PlaceComments where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Reference hiding (element)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import ApiAnnotation (ApiAnnKey, AnnotationComment(..))
import SrcLoc

import Language.Haskell.Tools.AST

getNormalComments :: Map SrcSpan [Located AnnotationComment] -> Map.Map SrcSpan [Located AnnotationComment]
getNormalComments = Map.map (filter (not . isPragma . unLoc))

getPragmaComments :: Map SrcSpan [Located AnnotationComment] -> Map.Map String [Located String]
getPragmaComments comms = Map.fromListWith (++) $ map (\(L l (AnnBlockComment str)) -> (getPragmaCommand str, [L l str]))
                                                $ filter (isPragma . unLoc) $ concatMap snd $ Map.toList comms
  where getPragmaCommand = takeWhile (\c -> isAlphaNum c || c == '_') . dropWhile isSpace . drop 3

isPragma :: AnnotationComment -> Bool
isPragma (AnnBlockComment str) = take 3 str == "{-#" && take 3 (reverse str) == "}-#"
isPragma _ = False

-- | Puts comments in the nodes they should be attached to. Watches for lexical tokens
-- that may divide the comment and the supposed element.
-- Leaves the AST in a state where parent nodes does not contain all of their children.
placeComments :: RangeInfo stage => Map ApiAnnKey [SrcSpan] -> Map.Map SrcSpan [Located AnnotationComment]
              -> Ann UModule dom stage -> Ann UModule dom stage
placeComments tokens comms mod
  = resizeAnnots (Set.filter (\rng -> srcSpanStart rng /= srcSpanEnd rng) $ Set.fromList $ concat (Map.elems tokens))
      (concatMap (map nextSrcLoc . snd) (Map.toList comms)) mod
  where spans = allElemSpans mod
        sortedElemStarts = Set.fromList $ map srcSpanStart spans
        sortedElemEnds = Set.fromList $ map srcSpanEnd spans
        nextSrcLoc comm@(L sp _)
          = let after = fromMaybe noSrcLoc (Set.lookupLE (srcSpanStart sp) sortedElemEnds)
                before = fromMaybe noSrcLoc (Set.lookupGE (srcSpanEnd sp) sortedElemStarts)
             in ((after,before),comm)

allElemSpans :: (SourceInfoTraversal node, RangeInfo stage) => Ann node dom stage -> [SrcSpan]
allElemSpans = execWriter . sourceInfoTraverse (SourceInfoTrf (\ni -> tell [ni ^. nodeSpan] >> pure ni) pure pure)

resizeAnnots :: RangeInfo stage => Set SrcSpan -> [((SrcLoc, SrcLoc), Located AnnotationComment)]
              -> Ann UModule dom stage
              -> Ann UModule dom stage
resizeAnnots tokens comments elem
  = flip evalState comments $ flip runReaderT tokens $
        -- if a comment that could be attached to more than one documentable element (possibly nested)
        -- the order of different documentable elements here decide which will be chosen

        modImports&annList !~ expandAnnot -- expand imports to cover their comments
          >=> modDecl&annList !~ expandTopLevelDecl -- expand declarations to cover their comments
          >=> expandAnnot -- expand the module itself to cover its comments
      $ elem

type ExpandType elem dom stage = Ann elem dom stage -> ReaderT (Set SrcSpan) (State [((SrcLoc, SrcLoc), Located AnnotationComment)]) (Ann elem dom stage)

expandTopLevelDecl :: RangeInfo stage => ExpandType UDecl dom stage
expandTopLevelDecl
  = declBody & annJust & cbElements & annList !~ expandClsElement
      >=> declCons & annList !~ expandConDecl
      >=> declGadt & annList !~ expandGadtConDecl
      >=> declTypeSig !~ expandTypeSig
      >=> expandAnnot

expandTypeSig :: RangeInfo stage => ExpandType UTypeSignature dom stage
expandTypeSig
  = tsType & typeParams !~ expandAnnot >=> expandAnnot

expandClsElement :: RangeInfo stage => ExpandType UClassElement dom stage
expandClsElement
  = ceTypeSig !~ expandTypeSig
      >=> ceBind !~ expandValueBind
      >=> expandAnnot

expandValueBind :: RangeInfo stage => ExpandType UValueBind dom stage
expandValueBind
  = valBindLocals & annJust & localBinds & annList !~ expandLocalBind
      >=> funBindMatches & annList & matchBinds & annJust & localBinds & annList !~ expandLocalBind
      >=> expandAnnot

expandLocalBind :: RangeInfo stage => ExpandType ULocalBind dom stage
expandLocalBind
  = localVal !~ expandValueBind
      >=> localSig !~ expandTypeSig
      >=> expandAnnot

expandConDecl :: RangeInfo stage => ExpandType UConDecl dom stage
expandConDecl
  = conDeclFields & annList !~ expandAnnot >=> expandAnnot

expandGadtConDecl :: RangeInfo stage => ExpandType UGadtConDecl dom stage
expandGadtConDecl
  = gadtConType & gadtConRecordFields & annList !~ expandAnnot >=> expandAnnot

-- | Expands tree elements to contain the comments that should be attached to them.
expandAnnot :: forall elem dom stage . RangeInfo stage => ExpandType elem dom stage
expandAnnot elem
  = do let Just sp = elem ^? annotation&sourceInfo&nodeSpan
       tokens <- ask
       applicable <- lift $ gets (applicableComments tokens (srcSpanStart sp) (srcSpanEnd sp))

       -- this check is just for performance (quick return if no modification is needed)
       if not (null applicable) then do
         -- the new span is the original plus all the covered spans
         let newSp@(RealSrcSpan newSpan)
               = foldl combineSrcSpans (fromJust $ elem ^? nodeSp) (map (getLoc . snd) applicable)
         -- take out all comments that are now covered
         lift $ modify (filter (not . (\case RealSrcSpan s -> newSpan `containsSpan` s; _ -> True) . getLoc . snd))
         return $ nodeSp .= newSp $ elem
       else return elem
  where nodeSp :: Simple Partial (Ann elem dom stage) SrcSpan
        nodeSp = annotation&sourceInfo&nodeSpan

-- This classification does not prefer inline comments to previous line comments, this is implicitly done
-- by the order in which the elements are traversed.
applicableComments :: Set SrcSpan -> SrcLoc -> SrcLoc
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)]
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)]
applicableComments tokens start end = filter applicableComment
  where -- A comment that starts with | binds to the next documented element
        applicableComment ((_, before), L sp comm)
          | isCommentOnNext comm = before == start && noTokenBetween (srcSpanEnd sp) start
        -- A comment that starts with ^ binds to the previous documented element
        applicableComment ((after, _), L sp comm)
          | isCommentOnPrev comm = after == end && noTokenBetween end (srcSpanStart sp)
        -- All other comment binds to the previous definition if it is on the same line
        applicableComment ((after, _), L sp@(RealSrcSpan loc) _)
          | after == end && srcLocLine (realSrcSpanStart loc) == getLineLocDefault end = True
                         && noTokenBetween end (srcSpanStart sp)
        -- or the next one if that is on the next line and the columns line up
        applicableComment ((_, before), L sp@(RealSrcSpan loc) _)
          | before == start && srcLocLine (realSrcSpanEnd loc) + 1 == getLineLocDefault start
                            && srcLocCol (realSrcSpanStart loc) == getLineColDefault start
                            && noTokenBetween (srcSpanEnd sp) start
          = True
        applicableComment _ = False

        getLineLocDefault (RealSrcLoc l) = srcLocLine l
        getLineLocDefault _              = -1

        getLineColDefault (RealSrcLoc l) = srcLocCol l
        getLineColDefault _              = -1

        noTokenBetween start end
          = case Set.lookupGE (srcLocSpan start) tokens of
              Just tok -> srcSpanStart tok >= end
              Nothing -> True

-- * GHC mistakenly parses -- ^ and -- | comments as simple line comments.
-- These functions check if a given comment is attached to the previous or next comment.

-- | Checks if a doc comment belongs to the next definition.
isCommentOnNext :: AnnotationComment -> Bool
isCommentOnNext (AnnDocCommentNext _) = True
isCommentOnNext (AnnLineComment s) = firstNonspaceCharIs '|' s
isCommentOnNext (AnnBlockComment s) = firstNonspaceCharIs '|' s
isCommentOnNext _ = False

-- | Checks if a doc comment belongs to the previous definition.
isCommentOnPrev :: AnnotationComment -> Bool
isCommentOnPrev (AnnDocCommentPrev _) = True
isCommentOnPrev (AnnLineComment s) = firstNonspaceCharIs '^' s
isCommentOnPrev (AnnBlockComment s) = firstNonspaceCharIs '^' s
isCommentOnPrev _ = False

-- the comment string contains the -- or {- characters
firstNonspaceCharIs :: Char -> String -> Bool
firstNonspaceCharIs c s = Just c == listToMaybe (dropWhile isSpace (drop 2 s))
