{-# LANGUAGE ScopedTypeVariables
           , StandaloneDeriving
           , DeriveDataTypeable 
           #-}
module Language.Haskell.Tools.AnnTrf.PlaceComments where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
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

deriving instance Data SrcLoc

-- created a correct Data instance for RealSrcLoc, because it is not given
instance Data RealSrcLoc where
  gfoldl k z loc = z mkRealSrcLoc `k` srcLocFile loc `k` srcLocLine loc `k` srcLocCol loc
  gunfold k z c = case constrIndex c of
                        1 -> k (k (k (z mkRealSrcLoc)))
  toConstr _ = con_RealSrcLoc
  dataTypeOf _ = ty_RealSrcLoc

con_RealSrcLoc = mkConstr ty_RealSrcLoc "RealSrcLoc" [] Prefix
ty_RealSrcLoc = mkDataType "SrcLoc.RealSrcLoc" [con_RealSrcLoc]
  
deriving instance Data SpanInfo

placeComments :: (StructuralTraversable node, Data sema, Data (node (NodeInfo sema SpanInfo)), Typeable node)
              => Map.Map SrcSpan [Located AnnotationComment] 
              -> Ann node (NodeInfo sema SpanInfo) 
              -> Ann node (NodeInfo sema SpanInfo)
placeComments comms mod
  = resizeAnnots (concatMap (map nextSrcLoc . snd) (Map.toList comms)) mod
  where sortedElemStarts = Set.fromList $ map srcSpanStart (allElemSpans mod)
        sortedElemEnds = Set.fromList $ map srcSpanEnd (allElemSpans mod)
        nextSrcLoc comm@(L sp _) 
          = let after = case Set.lookupLE (srcSpanEnd sp) sortedElemEnds of Just x  -> x
                                                                            Nothing -> noSrcLoc
                before = case Set.lookupGE (srcSpanStart sp) sortedElemStarts of Just x  -> x
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
  = flip evalState comments $ transformBiM (expandAnnot :: ExpandType Module sema)
                                >=> transformBiM (expandAnnot :: ExpandType ImportDecl sema)
                                >=> transformBiM (expandAnnot :: ExpandType Decl sema)
                                >=> transformBiM (expandAnnot :: ExpandType ClassElement sema)
                                >=> transformBiM (expandAnnot :: ExpandType ConDecl sema)
                                >=> transformBiM (expandAnnot :: ExpandType FieldDecl sema)
                                >=> transformBiM (expandAnnot :: ExpandType LocalBind sema)
                                -- >=> transformBiM expandAnnotToFunArgs
                            $ elem

type ExpandType elem sema = Ann elem (NodeInfo sema SpanInfo) 
                              -> State [((SrcLoc, SrcLoc), Located AnnotationComment)]
                                       (Ann elem (NodeInfo sema SpanInfo))

expandAnnot :: ExpandType elem sema
expandAnnot elem
  = do let Just sp = elem ^? annotation&sourceInfo&nodeSpan
       applicable <- gets (applicableComments (srcSpanStart sp) (srcSpanEnd sp))
       -- TODO: remove all comments covered by the combined src span
       return $ foldl addCommentToNode elem (map snd applicable)
  where addCommentToNode elem comm = annotation&sourceInfo&nodeSpan .- combineSrcSpans (getLoc comm) $ elem
        
-- This classification does not prefer inline comments to previous line comments, this is implicitely done
-- by the order in which the elements are traversed.
applicableComments :: SrcLoc -> SrcLoc 
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)] 
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)]
applicableComments start end comments = filter applicableComment comments
  where -- A comment that starts with | binds to the next documented element
        applicableComment ((_, before), L _ comm) 
          | isCommentOnNext comm = trace ("next: " ++ showSDocUnsafe (ppr (comm, before, start))) $ before == start
        -- A comment that starts with ^ binds to the previous documented element
        applicableComment ((after, _), L _ comm) 
          | isCommentOnPrev comm = trace ("prev: " ++ showSDocUnsafe (ppr (comm, after, end))) $ after == end
        -- All other comment binds to the previous definition if it is on the same line
        -- or the next one if that is on the next line
        applicableComment ((after, _), L (RealSrcSpan loc) _) 
          | after == end && (srcLocLine $ realSrcSpanStart loc) == getLineLocDefault end = trace ("simple ONLINE") $ True
        applicableComment ((_, before), L (RealSrcSpan loc) _) 
          | before == start && (srcLocLine $ realSrcSpanEnd loc) + 1 == getLineLocDefault start = trace ("simple PREVLINE") $ True
        applicableComment comm = trace ("simple: " ++ showSDocUnsafe (ppr (comm,start,end))) $ False
        
        getLineLocDefault (RealSrcLoc l) = srcLocLine l
        getLineLocDefault _              = -1

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

-- expandAnnotToFunArgs :: ExpandType TypeSignature sema
-- expandAnnotToFunArgs ts = element&tsType&typeParams !~ expandAnnot ts