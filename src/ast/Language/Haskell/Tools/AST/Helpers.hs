{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           , RankNTypes 
           , ScopedTypeVariables 
           #-}

-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import SrcLoc
import qualified Name as GHC

import Control.Reference hiding (element)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Function hiding ((&))
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.References

import Debug.Trace

ordByOccurrence :: SimpleName a -> SimpleName a -> Ordering
ordByOccurrence = compare `on` nameElements

-- | The occurrence of the name.
nameString :: SimpleName a -> String
nameString = intercalate "." . nameElements

-- | The qualifiers and the unqualified name
nameElements :: SimpleName a -> [String]
nameElements n = (n ^? qualifiers&annList&element&simpleNameStr) 
                    ++ [n ^. unqualifiedName&element&simpleNameStr]

-- | The qualifier of the name
nameQualifier :: SimpleName a -> [String]
nameQualifier n = n ^? qualifiers&annList&element&simpleNameStr
         
-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: ImportDecl a -> Bool
importIsExact = isJust . (^? importSpec&annJust&element&importSpecList)  
  
-- | Does the import declaration has a 'hiding' clause?
importIsHiding :: ImportDecl a -> Bool
importIsHiding = isJust . (^? importSpec&annJust&element&importSpecHiding)
       
-- | All elements that are explicitly listed to be imported in the import declaration
importExacts :: Simple Traversal (ImportDecl a) (IESpec a)
importExacts = importSpec&annJust&element&importSpecList&annList&element

-- | All elements that are hidden in an import
importHidings :: Simple Traversal (ImportDecl a) (IESpec a)
importHidings = importSpec&annJust&element&importSpecList&annList&element
         
-- | Possible qualifiers to use imported definitions         
importQualifiers :: ImportDecl a -> [[String]]
importQualifiers imp 
  = (if isAnnNothing (imp ^. importQualified) then [[]] else [])
      ++ maybe [] (\n -> [nameElements n]) 
               (imp ^? importAs&annJust&element&importRename&element)
        
bindingName :: Simple Traversal (Ann ValueBind (NodeInfo (SemanticInfo n) s)) n
bindingName = element&(valBindPat&element&patternName&element&simpleName 
                        &+& funBindMatches&annList&element&matchPattern&element
                              &(patternName&element&simpleName &+& patternOperator&element&operatorName))
                     &semantics&nameInfo
                     
declHeadNames :: Simple Traversal (Ann DeclHead a) (Ann SimpleName a)
declHeadNames = element & (dhName&element&simpleName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames &+& dhOperator&element&operatorName)

               
typeParams :: Simple Traversal (Ann Type a) (Ann Type a)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (TyFun p r)) = Ann a <$> (TyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (TyForall vs ctx t)) = Ann a <$> (TyForall vs ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyCtx ctx t)) = Ann a <$> (TyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyParen t)) = Ann a <$> (TyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t
        

-- | Access the semantic information of an AST node.
semantics :: Simple Lens (Ann a (NodeInfo sema src)) sema
semantics = annotation&semanticInfo

-- | Gets the GHC unique name of an AST node
getNameInfo :: (GHC.NamedThing n) => Ann e (NodeInfo (SemanticInfo n) src) -> Maybe GHC.Name 
getNameInfo e = getNameInfoFromSema (e ^. semantics)

getNameInfoFromSema :: (GHC.NamedThing n) => SemanticInfo n -> Maybe GHC.Name 
getNameInfoFromSema e = fmap GHC.getName (e ^? nameInfo) `mplus` (e ^? onlyNameInfo)

dhNames :: Simple Traversal (Ann DeclHead (NodeInfo (SemanticInfo n) src)) n
dhNames = declHeadNames & semantics & nameInfo


-- | A type class for transformations that work on both top-level and local definitions
class BindingElem d where
  sigBind :: Simple Partial (d a) (Ann TypeSignature a)
  valBind :: Simple Partial (d a) (Ann ValueBind a)
  createTypeSig :: Ann TypeSignature a -> d a
  createBinding :: Ann ValueBind a -> d a
  isTypeSig :: d a -> Bool
  isBinding :: d a -> Bool
  
instance BindingElem Decl where
  sigBind = declTypeSig
  valBind = declValBind
  createTypeSig = TypeSigDecl
  createBinding = ValueBinding
  isTypeSig (TypeSigDecl _) = True
  isTypeSig _ = False
  isBinding (ValueBinding _) = True
  isBinding _ = False

instance BindingElem LocalBind where
  sigBind = localSig
  valBind = localVal
  createTypeSig = LocalSignature
  createBinding = LocalValBind
  isTypeSig (LocalSignature _) = True
  isTypeSig _ = False
  isBinding (LocalValBind _) = True
  isBinding _ = False

bindName :: BindingElem d => Simple Traversal (d (NodeInfo (SemanticInfo n) src)) n
bindName = valBind&bindingName &+& sigBind&element&tsName&annList&element&simpleName&semantics&nameInfo

valBindsInList :: BindingElem d => Simple Traversal (AnnList d a) (Ann ValueBind a)
valBindsInList = annList & element & valBind
     
getValBindInList :: (BindingElem d, HasRange a) => RealSrcSpan -> AnnList d a -> Maybe (Ann ValueBind a)
getValBindInList sp ls = case ls ^? valBindsInList & filtered (isInside sp) of
  [] -> Nothing
  [n] -> Just n
  _ -> error "getValBindInList: Multiple nodes"

nodesContaining :: forall node inner a . (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
                => RealSrcSpan -> Simple Traversal (node a) (inner a)
nodesContaining rng = biplateRef & filtered (isInside rng) 
              
isInside :: (HasAnnot node, HasRange a) => RealSrcSpan -> node a -> Bool
isInside rng nd = case getRange (getAnnot nd) of RealSrcSpan sp -> sp `containsSpan` rng
                                                 _ -> False
             
nodesWithRange :: forall node inner a . (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
               => RealSrcSpan -> Simple Traversal (node a) (inner a)
nodesWithRange rng = biplateRef & filtered (hasRange rng) 
                                         
hasRange :: (HasAnnot node, HasRange a) => RealSrcSpan -> node a -> Bool
hasRange rng node = case getRange (getAnnot node) of RealSrcSpan sp -> sp == rng
                                                     _ -> False

getNodeContaining :: (Biplate (node a) (Ann inner a), HasAnnot node, HasRange a) 
                  => RealSrcSpan -> node a -> Maybe (Ann inner a)
getNodeContaining sp node = case node ^? nodesContaining sp of
  [] -> Nothing
  results -> Just $ minimumBy (compareRangeLength `on` (getRange . (^. annotation))) results

compareRangeLength :: SrcSpan -> SrcSpan -> Ordering
compareRangeLength (RealSrcSpan sp1) (RealSrcSpan sp2)
  = (lineDiff sp1 `compare` lineDiff sp2) `mappend` (colDiff sp1 `compare` colDiff sp2)
  where lineDiff sp = srcLocLine (realSrcSpanStart sp) - srcLocLine (realSrcSpanEnd sp)
        colDiff sp = srcLocCol (realSrcSpanStart sp) - srcLocCol (realSrcSpanEnd sp)

getNode :: (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
        => RealSrcSpan -> node a -> inner a
getNode sp node = case node ^? nodesWithRange sp of
  [] -> error "getNode: The node cannot be found"
  [n] -> n
  _ -> error "getNode: Multiple nodes"
