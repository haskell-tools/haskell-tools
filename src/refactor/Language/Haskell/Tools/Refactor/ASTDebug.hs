{-# LANGUAGE TypeOperators
           , DefaultSignatures
           , StandaloneDeriving
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , TemplateHaskell
           , OverloadedStrings
           , LambdaCase
           #-}
module Language.Haskell.Tools.Refactor.ASTDebug where

import GHC.Generics
import Control.Reference
import Control.Applicative
import Data.Sequence as Seq
import Data.Foldable
import Data.List as List
import SrcLoc

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate




data DebugNode a = TreeNode { _nodeSubtree :: TreeDebugNode a 
                            }
                 | SimpleNode { _nodeLabel :: String
                              , _nodeValue :: String
                              }
  deriving Show

data TreeDebugNode a
  = TreeDebugNode { _nodeName :: String
                  , _nodeInfo :: a
                  , _children :: [DebugNode a]
                  }
  deriving Show

makeReferences ''DebugNode
makeReferences ''TreeDebugNode

astDebug :: ASTDebug e a => e a -> String
astDebug ast = toList (astDebugToJson (astDebug' ast))

astDebugToJson :: [DebugNode a] -> Seq Char
astDebugToJson nodes = fromList "[ " >< childrenJson >< fromList " ]"
    where (treeNodes, otherNodes) = List.partition (\case TreeNode {} -> True; _ -> False) nodes
          childrenJson = case map (astDebugElemJson . _nodeSubtree) treeNodes of 
                           first:rest -> first >< foldl (><) Seq.empty (fmap (fromList ", " ><) (fromList rest))
                           []         -> Seq.empty

astDebugElemJson :: TreeDebugNode a -> Seq Char
astDebugElemJson (TreeDebugNode name info children) 
  = fromList "{ \"text\" : \"" >< fromList name >< fromList "\", \"children\" : " >< astDebugToJson children >< fromList " }"

class ASTDebug e a where
  astDebug' :: e a -> [DebugNode a]
  default astDebug' :: (GAstDebug (Rep (e a)) a, Generic (e a)) => e a -> [DebugNode a]
  astDebug' = gAstDebug . from

class GAstDebug f a where 
  gAstDebug :: f p -> [DebugNode a]
  
instance GAstDebug V1 a where
  gAstDebug _ = error "GAstDebug V1"
  
instance GAstDebug U1 a where
  gAstDebug U1 = []  
  
instance (GAstDebug f a, GAstDebug g a) => GAstDebug (f :+: g) a where
  gAstDebug (L1 x) = gAstDebug x
  gAstDebug (R1 x) = gAstDebug x
  
instance (GAstDebug f a, GAstDebug g a) => GAstDebug (f :*: g) a where
  gAstDebug (x :*: y) 
    = gAstDebug x ++ gAstDebug y

instance {-# OVERLAPPING #-} ASTDebug e a => GAstDebug (K1 i (e a)) a where
  gAstDebug (K1 x) = astDebug' x
  
instance {-# OVERLAPPABLE #-} Show x => GAstDebug (K1 i x) a where
  gAstDebug (K1 x) = [SimpleNode "" (show x)]
        
instance (GAstDebug f a, Constructor c) => GAstDebug (M1 C c f) a where
  gAstDebug c@(M1 x) = [TreeNode (TreeDebugNode (conName c) undefined (gAstDebug x))]

instance (GAstDebug f a, Selector s) => GAstDebug (M1 S s f) a where
  gAstDebug s@(M1 x) = traversal&nodeLabel .= selName s $ gAstDebug x

-- don't have to do anything with datatype metainfo
instance GAstDebug f a => GAstDebug (M1 D t f) a where
  gAstDebug (M1 x) = gAstDebug x
