{-# LANGUAGE TypeOperators
           , DefaultSignatures
           , StandaloneDeriving
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , TemplateHaskell
           #-}
module Language.Haskell.Tools.Refactor.ASTDebug where

import GHC.Generics
import Control.Reference
import Control.Applicative
import SrcLoc

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate

data DebugNode a = TreeNode { _nodeLabel :: String
                            , _nodeSubtree :: TreeDebugNode a
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

astDebug :: ASTDebug e a => e a -> [DebugNode a]
astDebug = astDebug'

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
  gAstDebug c@(M1 x) = [TreeNode "" (TreeDebugNode "" undefined (gAstDebug x))]

instance (GAstDebug f a, Selector s) => GAstDebug (M1 S s f) a where
  gAstDebug s@(M1 x) = traversal&nodeLabel .= selName s $ gAstDebug x

-- don't have to do anything with datatype metainfo
instance GAstDebug f a => GAstDebug (M1 D t f) a where
  gAstDebug (M1 x) = gAstDebug x
