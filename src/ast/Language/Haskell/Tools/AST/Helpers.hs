{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           , RankNTypes 
           , ScopedTypeVariables 
           #-}

-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import qualified SrcLoc as GHC
import qualified Name as GHC

import Control.Reference hiding (element)
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

ordByOccurrence :: Name a -> Name a -> Ordering
ordByOccurrence = compare `on` nameElements

-- | The occurrence of the name.
nameString :: Name a -> String
nameString = concat . intersperse "." . nameElements

-- | The qualifiers and the unqualified name
nameElements :: Name a -> [String]
nameElements n = (n ^? qualifiers&annList&element&simpleNameStr) 
                    ++ [n ^. unqualifiedName&element&simpleNameStr]

-- | The qualifier of the name
nameQualifier :: Name a -> [String]
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
bindingName = element&(valBindPat&element&patternVar &+& funBindMatches&annList&element&matchName)
                     &semantics&nameInfo
                     
declHeadNames :: Simple Traversal (Ann DeclHead a) (Ann Name a)
declHeadNames = element & (dhName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames)

               
typeParams :: Simple Traversal (Ann Type a) (Ann Type a)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (TyFun p r)) = Ann a <$> (TyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (TyForall vs ctx t)) = Ann a <$> (TyForall vs ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyCtx ctx t)) = Ann a <$> (TyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyParen t)) = Ann a <$> (TyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t
        

        
semantics :: Simple Lens (Ann a (NodeInfo sema src)) sema
semantics = annotation&semanticInfo

-- | A type class for transformations that work on both top-level and local definitions
class BindingElem d where
  bindName :: Simple Traversal (d (NodeInfo (SemanticInfo n) src)) n
  createTypeSig :: Ann TypeSignature a -> d a
  createBinding :: Ann ValueBind a -> d a
  isTypeSig :: d a -> Bool
  isBinding :: d a -> Bool
  
instance BindingElem Decl where
  bindName = declValBind&bindingName &+& declTypeSig&element&tsName&annList&semantics&nameInfo
  createTypeSig = TypeSigDecl
  createBinding = ValueBinding
  isTypeSig (TypeSigDecl _) = True
  isTypeSig _ = False
  isBinding (ValueBinding _) = True
  isBinding _ = False

instance BindingElem LocalBind where
  bindName = localVal&bindingName &+& localSig&element&tsName&annList&semantics&nameInfo
  createTypeSig = LocalSignature
  createBinding = LocalValBind
  isTypeSig (LocalSignature _) = True
  isTypeSig _ = False
  isBinding (LocalValBind _) = True
  isBinding _ = False
              
     
nodesInside :: forall node inner a . (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
            => GHC.RealSrcSpan -> Simple Traversal (node a) (inner a)
nodesInside rng = biplateRef & filtered (isInside rng) 
              
isInside :: (HasAnnot node, HasRange a) => GHC.RealSrcSpan -> node a -> Bool
isInside rng nd = case getRange (getAnnot nd) of GHC.RealSrcSpan sp -> sp `GHC.containsSpan` rng
                                                 _ -> False
             
nodesWithRange :: forall node inner a . (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
               => GHC.RealSrcSpan -> Simple Traversal (node a) (inner a)
nodesWithRange rng = biplateRef & filtered (hasRange rng) 
                                         
hasRange :: (HasAnnot node, HasRange a) => GHC.RealSrcSpan -> node a -> Bool
hasRange rng node = case getRange (getAnnot node) of GHC.RealSrcSpan sp -> sp == rng
                                                     _ -> False


getNode :: (Biplate (node a) (inner a), HasAnnot node, HasAnnot inner, HasRange a) 
        => GHC.RealSrcSpan -> node a -> inner a
getNode sp node = case node ^? nodesWithRange sp of
  [] -> error "getNode: The node cannot be found"
  [n] -> n
  _ -> error "getNode: Multiple nodes"
