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
           , ViewPatterns
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.Refactor.ASTDebug where

import GHC.Generics
import Control.Reference
import Control.Applicative
import Data.Sequence as Seq
import Data.Foldable
import Data.Maybe
import Data.List as List
import SrcLoc
import Outputable

import DynFlags as GHC
import Name as GHC
import Id as GHC
import RdrName as GHC
import Unique as GHC

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.Refactor.RangeDebug


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

astDebug :: (InspectableName n, HasRange a, ASTDebug e a, a ~ (NodeInfo (SemanticInfo n) src)) => e a -> String
astDebug ast = toList (astDebugToJson (astDebug' ast))

astDebugToJson :: (InspectableName n, HasRange a, a ~ (NodeInfo (SemanticInfo n) src)) => [DebugNode a] -> Seq Char
astDebugToJson nodes = fromList "[ " >< childrenJson >< fromList " ]"
    where treeNodes = List.filter (\case TreeNode {} -> True; _ -> False) nodes
          childrenJson = case map debugTreeNode treeNodes of 
                           first:rest -> first >< foldl (><) Seq.empty (fmap (fromList ", " ><) (fromList rest))
                           []         -> Seq.empty
          debugTreeNode (TreeNode "" s) = astDebugElemJson s
          debugTreeNode (TreeNode (dropWhile (=='_') -> l) s) = astDebugElemJson (nodeName .- (("<span class='astlab'>" ++ l ++ "</span>: ") ++) $ s)

astDebugElemJson :: (InspectableName n, HasRange a, a ~ (NodeInfo (SemanticInfo n) src)) => TreeDebugNode a -> Seq Char
astDebugElemJson (TreeDebugNode name info children) 
  = fromList "{ \"text\" : \"" >< fromList name 
     >< fromList "\", \"state\" : { \"opened\" : true }, \"a_attr\" : { \"data-range\" : \"" 
     >< fromList (shortShowSpan (getRange info))
     >< fromList "\", \"data-elems\" : \"" 
     >< foldl (><) Seq.empty dataElems
     >< fromList "\", \"data-sema\" : \"" 
     >< fromList (showSema (info ^. semanticInfo))
     >< fromList "\" }, \"children\" : " 
     >< astDebugToJson children >< fromList " }"
  where dataElems = catMaybes (map (\case SimpleNode l v -> Just (fromList (formatScalarElem l v)); _ -> Nothing) children)
        formatScalarElem l v = "<div class='scalarelem'><span class='astlab'>" ++ l ++ "</span>: " ++ tail (init (show v)) ++ "</div>"
        showSema info = "<div class='semaname'>" ++ assocName info ++ "</div>" 
                          ++ concatMap (\(l,i) -> "<div class='scalarelem'><span class='astlab'>" ++ l ++ "</span>: " ++ i ++ "</div>") (toAssoc info) 

class AssocData a where
  assocName :: a -> String
  toAssoc :: a -> [(String, String)]

instance InspectableName n => AssocData (SemanticInfo n) where
  assocName NoSemanticInfo = "NoSemanticInfo"
  assocName (ScopeInfo {}) = "ScopeInfo"
  assocName (NameInfo {}) = "NameInfo"
  assocName (ModuleInfo {}) = "ModuleInfo"
  assocName (ImportInfo {}) = "ImportInfo"

  toAssoc NoSemanticInfo = []
  toAssoc (ScopeInfo locals) = [ ("namesInScope", inspectScope locals) ]
  toAssoc (NameInfo locals defined nameInfo) = [ ("name", inspect nameInfo)
                                               , ("isDefined", show defined)
                                               , ("namesInScope", inspectScope locals) 
                                               ]
  toAssoc (ModuleInfo mod) = [("moduleName", showSDocUnsafe (ppr mod))]
  toAssoc (ImportInfo mod avail imported) = [ ("moduleName", showSDocUnsafe (ppr mod)) 
                                            , ("availableNames", concat (intersperse ", " (map inspect avail))) 
                                            , ("importedNames", concat (intersperse ", " (map inspect imported))) 
                                            ]

inspectScope :: InspectableName n => [[n]] -> String
inspectScope = concat . intersperse " | " . map (concat . intersperse ", " . map inspect)

class InspectableName n where
  inspect :: n -> String

instance InspectableName GHC.Name where
  inspect name = showSDocUnsafe (ppr name) ++ "[" ++ show (getUnique name) ++ "]"

instance InspectableName GHC.RdrName where
  inspect name = showSDocUnsafe (ppr name)

instance InspectableName GHC.Id where
  inspect name = showSDocUnsafe (ppr name) ++ "[" ++ show (getUnique name) ++ "] :: " ++ showSDocOneLine unsafeGlobalDynFlags (ppr (idType name))

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
  gAstDebug c@(M1 x) = [TreeNode "" (TreeDebugNode (conName c) undefined (gAstDebug x))]

instance (GAstDebug f a, Selector s) => GAstDebug (M1 S s f) a where
  gAstDebug s@(M1 x) = traversal&nodeLabel .= selName s $ gAstDebug x

-- don't have to do anything with datatype metainfo
instance GAstDebug f a => GAstDebug (M1 D t f) a where
  gAstDebug (M1 x) = gAstDebug x
