{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           #-}

-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import Control.Reference hiding (element)
import Data.List
import Data.Maybe
import Data.Function hiding ((&))

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Modules
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

typeParams :: Simple Traversal (Ann Type a) (Ann Type a)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (TyFun p r)) = Ann a <$> (TyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (TyForall vs ctx t)) = Ann a <$> (TyForall vs ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyCtx ctx t)) = Ann a <$> (TyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (TyParen t)) = Ann a <$> (TyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t
                   