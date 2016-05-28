{-# LANGUAGE ScopedTypeVariables
           , StandaloneDeriving
           , DeriveDataTypeable 
           , FlexibleContexts 
           , LambdaCase 
           #-}
module Language.Haskell.Tools.AnnTrf.PlaceComments where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.String.Utils
import Data.Char (isSpace, isAlphaNum)
import Data.Data
import Data.Generics.Uniplate.Data
import Control.Reference hiding (element)
import Control.Monad.Writer
import Control.Monad.State
import Data.StructuralTraversal

import SrcLoc
import ApiAnnotation
import Outputable
import Debug.Trace

import Language.Haskell.Tools.AST

getNormalComments :: Map.Map SrcSpan [Located AnnotationComment] -> Map.Map SrcSpan [Located AnnotationComment] 
getNormalComments = Map.map (filter (not . isPragma . unLoc))

getPragmaComments :: Map.Map SrcSpan [Located AnnotationComment] -> Map.Map String [Located String]
getPragmaComments comms = Map.fromListWith (++) $ map (\(L l (AnnBlockComment str)) -> (getPragmaCommand str, [L l str])) 
                                                $ filter (isPragma . unLoc) $ concat $ map snd $ Map.toList comms 
  where getPragmaCommand = takeWhile isAlphaNum . dropWhile isSpace . drop 3

isPragma :: AnnotationComment -> Bool
isPragma (AnnBlockComment str) = startswith "{-#" str && endswith "#-}" str
isPragma _ = False

-- | Puts comments in the nodes they should be attached to. Leaves the AST in a state where parent nodes
-- does not contain all of their children.
placeComments :: (StructuralTraversable node, Data sema, Data (node (NodeInfo sema SpanInfo)), Typeable node)
              => Map.Map SrcSpan [Located AnnotationComment] 
              -> Ann node (NodeInfo sema SpanInfo) 
              -> Ann node (NodeInfo sema SpanInfo)
placeComments comms mod
  = resizeAnnots (concatMap (map nextSrcLoc . snd) (Map.toList comms)) mod
  where sortedElemStarts = Set.fromList $ map srcSpanStart (allElemSpans mod)
        sortedElemEnds = Set.fromList $ map srcSpanEnd (allElemSpans mod)
        nextSrcLoc comm@(L sp _) 
          = let after = case Set.lookupLE (srcSpanStart sp) sortedElemEnds of Just x  -> x
                                                                              Nothing -> noSrcLoc
                before = case Set.lookupGE (srcSpanEnd sp) sortedElemStarts of Just x  -> x
                                                                               Nothing -> noSrcLoc
             in ((after,before),comm)
  
allElemSpans :: StructuralTraversable node => Ann node (NodeInfo sema SpanInfo) -> [SrcSpan]
allElemSpans = execWriter . traverseDown (return ()) (return ()) 
                             (\ni -> maybe (return ()) (tell . (:[])) (ni ^? sourceInfo&nodeSpan))
                                                 
resizeAnnots :: forall node sema . (Data sema, Data (node (NodeInfo sema SpanInfo)), Typeable node) 
                  => [((SrcLoc, SrcLoc), Located AnnotationComment)]
                  -> Ann node (NodeInfo sema SpanInfo) 
                  -> Ann node (NodeInfo sema SpanInfo) 
resizeAnnots comments elem
  = flip evalState comments $ 
        -- if a comment that could be attached to more than one documentable element (possibly nested) 
        -- the order of different documentable elements here decide which will be chosen
        
        transformBiM (expandAnnot :: ExpandType ImportDecl sema)
          >=> transformBiM (expandAnnotToFunArgs :: ExpandType TypeSignature sema)
          >=> transformBiM (expandAnnot :: ExpandType Decl sema)
          >=> transformBiM (expandAnnot :: ExpandType ClassElement sema)
          >=> transformBiM (expandAnnot :: ExpandType ConDecl sema)
          >=> transformBiM (expandAnnot :: ExpandType FieldDecl sema)
          >=> transformBiM (expandAnnot :: ExpandType LocalBind sema)
          >=> transformBiM (expandAnnot :: ExpandType Module sema)
      $ elem

type ExpandType elem sema = Ann elem (NodeInfo sema SpanInfo) 
                              -> State [((SrcLoc, SrcLoc), Located AnnotationComment)]
                                       (Ann elem (NodeInfo sema SpanInfo))

expandAnnot :: forall elem sema . ExpandType elem sema
expandAnnot elem
  = do let Just sp = elem ^? annotation&sourceInfo&nodeSpan
       applicable <- gets (applicableComments (srcSpanStart sp) (srcSpanEnd sp))
       
       -- this check is just for performance (quick return if no modification is needed)
       if not (null applicable) then do
         -- the new span is the original plus all the covered spans
         let newSp@(RealSrcSpan newSpan) 
               = foldl combineSrcSpans (fromJust $ elem ^? nodeSp) (map (getLoc . snd) applicable)
         -- take out all comments that are now covered
         modify (filter (not . (\case RealSrcSpan s -> newSpan `containsSpan` s; _ -> True) . getLoc . snd))
         return $ nodeSp .= newSp $ elem
       else return elem
  where nodeSp :: Simple Partial (Ann elem (NodeInfo sema SpanInfo)) SrcSpan
        nodeSp = annotation&sourceInfo&nodeSpan
        
-- This classification does not prefer inline comments to previous line comments, this is implicitely done
-- by the order in which the elements are traversed.
applicableComments :: SrcLoc -> SrcLoc 
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)] 
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)]
applicableComments start end comments = filter applicableComment comments
  where -- A comment that starts with | binds to the next documented element
        applicableComment ((_, before), L _ comm) 
          | isCommentOnNext comm = before == start
        -- A comment that starts with ^ binds to the previous documented element
        applicableComment ((after, _), L _ comm) 
          | isCommentOnPrev comm = after == end
        -- All other comment binds to the previous definition if it is on the same line
        applicableComment ((after, _), L (RealSrcSpan loc) _) 
          | after == end && (srcLocLine $ realSrcSpanStart loc) == getLineLocDefault end = True
        -- or the next one if that is on the next line and the columns line up
        applicableComment ((_, before), L (RealSrcSpan loc) _) 
          | before == start && (srcLocLine $ realSrcSpanEnd loc) + 1 == getLineLocDefault start
                            && (srcLocCol $ realSrcSpanStart loc) == getLineColDefault start
          = True
        applicableComment _ = False
        
        getLineLocDefault (RealSrcLoc l) = srcLocLine l
        getLineLocDefault _              = -1

        getLineColDefault (RealSrcLoc l) = srcLocCol l
        getLineColDefault _              = -1

-- * GHC mistakenly parses -- ^ and -- | comments as simple line comments.
-- These functions check if a given comment is attached to the previous or next comment.

isCommentOnNext :: AnnotationComment -> Bool
isCommentOnNext (AnnDocCommentNext _) = True
isCommentOnNext (AnnLineComment s) = firstNonspaceCharIs '|' s
isCommentOnNext (AnnBlockComment s) = firstNonspaceCharIs '|' s
isCommentOnNext _ = False

isCommentOnPrev :: AnnotationComment -> Bool
isCommentOnPrev (AnnDocCommentPrev _) = True
isCommentOnPrev (AnnLineComment s) = firstNonspaceCharIs '^' s
isCommentOnPrev (AnnBlockComment s) = firstNonspaceCharIs '^' s
isCommentOnPrev _ = False

-- the comment string contains the -- or {- characters
firstNonspaceCharIs :: Char -> String -> Bool
firstNonspaceCharIs c s = Just c == listToMaybe (dropWhile isSpace (drop 2 s))

expandAnnotToFunArgs :: ExpandType TypeSignature sema
expandAnnotToFunArgs = element&tsType&typeParams !~ expandAnnot
