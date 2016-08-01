{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts 
           , LambdaCase 
           #-}
-- | This transformation expands nodes to contain the comments that should be attached to them. After this, a
-- normalizing transformation should be performed that expands parents to contain their children.
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

import SrcLoc
import ApiAnnotation
import Outputable
import Debug.Trace

import Language.Haskell.Tools.AST

getNormalComments :: Map.Map SrcSpan [Located AnnotationComment] -> Map.Map SrcSpan [Located AnnotationComment] 
getNormalComments = Map.map (filter (not . isPragma . unLoc))

getPragmaComments :: Map.Map SrcSpan [Located AnnotationComment] -> Map.Map String [Located String]
getPragmaComments comms = Map.fromListWith (++) $ map (\(L l (AnnBlockComment str)) -> (getPragmaCommand str, [L l str])) 
                                                $ filter (isPragma . unLoc) $ concatMap snd $ Map.toList comms 
  where getPragmaCommand = takeWhile (\c -> isAlphaNum c || c == '_') . dropWhile isSpace . drop 3

isPragma :: AnnotationComment -> Bool
isPragma (AnnBlockComment str) = startswith "{-#" str && endswith "#-}" str
isPragma _ = False

-- | Puts comments in the nodes they should be attached to. Leaves the AST in a state where parent nodes
-- does not contain all of their children.
placeComments :: RangeInfo stage => Map.Map SrcSpan [Located AnnotationComment] 
              -> Ann Module dom stage
              -> Ann Module dom stage
placeComments comms mod
  = resizeAnnots (concatMap (map nextSrcLoc . snd) (Map.toList comms)) mod
  where spans = allElemSpans mod
        sortedElemStarts = Set.fromList $ map srcSpanStart spans
        sortedElemEnds = Set.fromList $ map srcSpanEnd spans
        nextSrcLoc comm@(L sp _) 
          = let after = fromMaybe noSrcLoc (Set.lookupLE (srcSpanStart sp) sortedElemEnds)
                before = fromMaybe noSrcLoc (Set.lookupGE (srcSpanEnd sp) sortedElemStarts)
             in ((after,before),comm)
  
allElemSpans :: (SourceInfoTraversal node, RangeInfo stage) => Ann node dom stage -> [SrcSpan]
allElemSpans = execWriter . sourceInfoTraverse (SourceInfoTrf (\ni -> tell [ni ^. nodeSpan] >> pure ni) pure pure)
                                                 
resizeAnnots :: RangeInfo stage => [((SrcLoc, SrcLoc), Located AnnotationComment)]
              -> Ann Module dom stage
              -> Ann Module dom stage
resizeAnnots comments elem
  = flip evalState comments $ 
        -- if a comment that could be attached to more than one documentable element (possibly nested) 
        -- the order of different documentable elements here decide which will be chosen
        
        element&modImports&annList !~ expandAnnot -- expand imports to cover their comments
          >=> element&modDecl&annList !~ expandTopLevelDecl -- expand declarations to cover their comments
          >=> expandAnnot -- expand the module itself to cover its comments
      $ elem

type ExpandType elem dom stage = Ann elem dom stage -> State [((SrcLoc, SrcLoc), Located AnnotationComment)] (Ann elem dom stage)

expandTopLevelDecl :: RangeInfo stage => ExpandType Decl dom stage
expandTopLevelDecl
  = element & declBody & annJust & element & cbElements & annList !~ expandClsElement
      >=> element & declCons & annList !~ expandConDecl
      >=> element & declGadt & annList !~ expandGadtConDecl
      >=> element & declTypeSig !~ expandTypeSig
      >=> expandAnnot

expandTypeSig :: RangeInfo stage => ExpandType TypeSignature dom stage
expandTypeSig
  = element & tsType & typeParams !~ expandAnnot >=> expandAnnot

expandClsElement :: RangeInfo stage => ExpandType ClassElement dom stage
expandClsElement
  = element & ceTypeSig !~ expandTypeSig
      >=> element & ceBind !~ expandValueBind
      >=> expandAnnot

expandValueBind :: RangeInfo stage => ExpandType ValueBind dom stage
expandValueBind
  = element & valBindLocals & annJust & element & localBinds & annList !~ expandLocalBind 
      >=> element & funBindMatches & annList & element & matchBinds & annJust & element & localBinds & annList !~ expandLocalBind
      >=> expandAnnot

expandLocalBind :: RangeInfo stage => ExpandType LocalBind dom stage
expandLocalBind
  = element & localVal !~ expandValueBind 
      >=> element & localSig !~ expandTypeSig 
      >=> expandAnnot

expandConDecl :: RangeInfo stage => ExpandType ConDecl dom stage
expandConDecl
  = element & conDeclFields & annList !~ expandAnnot >=> expandAnnot

expandGadtConDecl :: RangeInfo stage => ExpandType GadtConDecl dom stage
expandGadtConDecl
  = element & gadtConType & element & gadtConRecordFields & annList !~ expandAnnot >=> expandAnnot

-- | Expands tree elements to contain the comments that should be attached to them.
expandAnnot :: forall elem dom stage . RangeInfo stage => ExpandType elem dom stage
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
  where nodeSp :: Simple Partial (Ann elem dom stage) SrcSpan
        nodeSp = annotation&sourceInfo&nodeSpan
        
-- This classification does not prefer inline comments to previous line comments, this is implicitely done
-- by the order in which the elements are traversed.
applicableComments :: SrcLoc -> SrcLoc 
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)] 
                   -> [((SrcLoc, SrcLoc), Located AnnotationComment)]
applicableComments start end = filter applicableComment
  where -- A comment that starts with | binds to the next documented element
        applicableComment ((_, before), L _ comm) 
          | isCommentOnNext comm = before == start
        -- A comment that starts with ^ binds to the previous documented element
        applicableComment ((after, _), L _ comm) 
          | isCommentOnPrev comm = after == end
        -- All other comment binds to the previous definition if it is on the same line
        applicableComment ((after, _), L (RealSrcSpan loc) _) 
          | after == end && srcLocLine (realSrcSpanStart loc) == getLineLocDefault end = True
        -- or the next one if that is on the next line and the columns line up
        applicableComment ((_, before), L (RealSrcSpan loc) _) 
          | before == start && srcLocLine (realSrcSpanEnd loc) + 1 == getLineLocDefault start
                            && srcLocCol (realSrcSpanStart loc) == getLineColDefault start
          = True
        applicableComment _ = False
        
        getLineLocDefault (RealSrcLoc l) = srcLocLine l
        getLineLocDefault _              = -1

        getLineColDefault (RealSrcLoc l) = srcLocCol l
        getLineColDefault _              = -1

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
