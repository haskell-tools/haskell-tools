{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           , RankNTypes 
           , ScopedTypeVariables
           , TypeFamilies
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
import Language.Haskell.Tools.AST.SemaInfoTypes

import Debug.Trace

ordByOccurrence :: QualifiedName dom stage -> QualifiedName dom stage -> Ordering
ordByOccurrence = compare `on` nameElements

-- | The occurrence of the name.
nameString :: QualifiedName dom stage -> String
nameString = intercalate "." . nameElements

-- | The qualifiers and the unqualified name
nameElements :: QualifiedName dom stage -> [String]
nameElements n = (n ^? qualifiers&annList&element&simpleNameStr) 
                    ++ [n ^. unqualifiedName&element&simpleNameStr]

-- | The qualifier of the name
nameQualifier :: QualifiedName dom stage -> [String]
nameQualifier n = n ^? qualifiers&annList&element&simpleNameStr
         
-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: ImportDecl dom stage -> Bool
importIsExact = isJust . (^? importSpec&annJust&element&importSpecList)  
  
-- | Does the import declaration has a 'hiding' clause?
importIsHiding :: ImportDecl dom stage -> Bool
importIsHiding = isJust . (^? importSpec&annJust&element&importSpecHiding)
       
-- | All elements that are explicitly listed to be imported in the import declaration
importExacts :: Simple Traversal (ImportDecl dom stage) (IESpec dom stage)
importExacts = importSpec&annJust&element&importSpecList&annList&element

-- | All elements that are hidden in an import
importHidings :: Simple Traversal (ImportDecl dom stage) (IESpec dom stage)
importHidings = importSpec&annJust&element&importSpecList&annList&element
         
-- | Possible qualifiers to use imported definitions         
importQualifiers :: ImportDecl dom stage -> [[String]]
importQualifiers imp 
  = (if isAnnNothing (imp ^. importQualified) then [[]] else [])
      ++ [imp ^? importAs&annJust&element&importRename&element&moduleNameString]
        
bindingName :: (SemanticInfo dom QualifiedName ~ ni) => Simple Traversal (Ann ValueBind dom stage) ni
bindingName = element&(valBindPat&element&patternName&element&simpleName 
                        &+& funBindMatches&annList&element&matchLhs&element
                              &(matchLhsName&element&simpleName &+& matchLhsOperator&element&operatorName))
                     &semantics
                     
declHeadNames :: Simple Traversal (Ann DeclHead dom stage) (Ann QualifiedName dom stage)
declHeadNames = element & (dhName&element&simpleName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames &+& dhOperator&element&operatorName)

               
typeParams :: Simple Traversal (Ann Type dom stage) (Ann Type dom stage)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (TyFun p r)) = Ann a <$> (TyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (TyForall vs t)) = Ann a <$> (TyForall vs <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyCtx ctx t)) = Ann a <$> (TyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyParen t)) = Ann a <$> (TyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t
        

-- | Access the semantic information of an AST node.
semantics :: Simple Lens (Ann elem dom stage) (SemanticInfo dom elem)
semantics = annotation&semanticInfo

dhNames :: (SemanticInfo dom QualifiedName ~ k) => Simple Traversal (Ann DeclHead dom stage) k
dhNames = declHeadNames & semantics

-- | A type class for transformations that work on both top-level and local definitions
class BindingElem d where
  sigBind :: Simple Partial (d dom stage) (Ann TypeSignature dom stage)
  valBind :: Simple Partial (d dom stage) (Ann ValueBind dom stage)
  createTypeSig :: Ann TypeSignature dom stage -> d dom stage
  createBinding :: Ann ValueBind dom stage -> d dom stage
  isTypeSig :: d dom stage -> Bool
  isBinding :: d dom stage -> Bool
  
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

bindName :: (BindingElem d, SemanticInfo dom QualifiedName ~ k) => Simple Traversal (d dom stage) k
bindName = valBind&bindingName &+& sigBind&element&tsName&annList&element&simpleName&semantics

valBindsInList :: BindingElem d => Simple Traversal (AnnList d dom stage) (Ann ValueBind dom stage)
valBindsInList = annList & element & valBind
     
getValBindInList :: (BindingElem d, SourceInfo stage) => RealSrcSpan -> AnnList d dom stage -> Maybe (Ann ValueBind dom stage)
getValBindInList sp ls = case ls ^? valBindsInList & filtered (isInside sp) of
  [] -> Nothing
  [n] -> Just n
  _ -> error "getValBindInList: Multiple nodes"

-- | Get all nodes that contain a given source range
nodesContaining :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage), SourceInfo stage) 
                => RealSrcSpan -> Simple Traversal (node dom stage) (inner dom stage)
nodesContaining rng = biplateRef & filtered (isInside rng) 

-- | Return true if the node contains a given range
isInside :: HasRange (inner dom stage) => RealSrcSpan -> inner dom stage -> Bool
isInside rng nd = case getRange nd of RealSrcSpan sp -> sp `containsSpan` rng
                                      _              -> False

-- | Get the nodes that have exactly the given range 
nodesWithRange :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage) 
               => RealSrcSpan -> Simple Traversal (Ann node dom stage) (Ann inner dom stage)
nodesWithRange rng = biplateRef & filtered (hasRange rng) 
                    
-- | True, if the node has the given range                     
hasRange :: SourceInfo stage => RealSrcSpan -> Ann inner dom stage -> Bool
hasRange rng node = case getRange node of RealSrcSpan sp -> sp == rng
                                          _              -> False

-- | Get the shortest source range that contains the given 
getNodeContaining :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage, HasRange (Ann inner dom stage)) 
                  => RealSrcSpan -> Ann node dom stage -> Maybe (Ann inner dom stage)
getNodeContaining sp node = case node ^? nodesContaining sp of
  [] -> Nothing
  results -> Just $ minimumBy (compareRangeLength `on` getRange) results

-- | Compares two NESTED source spans based on their lengths
compareRangeLength :: SrcSpan -> SrcSpan -> Ordering
compareRangeLength (RealSrcSpan sp1) (RealSrcSpan sp2)
  = (lineDiff sp1 `compare` lineDiff sp2) `mappend` (colDiff sp1 `compare` colDiff sp2)
  where lineDiff sp = srcLocLine (realSrcSpanStart sp) - srcLocLine (realSrcSpanEnd sp)
        colDiff sp = srcLocCol (realSrcSpanStart sp) - srcLocCol (realSrcSpanEnd sp)

getNode :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage) 
        => RealSrcSpan -> Ann node dom stage -> Ann inner dom stage
getNode sp node = case node ^? nodesWithRange sp of
  [] -> error "getNode: The node cannot be found"
  [n] -> n
  _ -> error "getNode: Multiple nodes"
