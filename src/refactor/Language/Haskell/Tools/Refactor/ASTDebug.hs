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
-- | A module for displaying the AST in a tree view.
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


data DebugNode dom = TreeNode { _nodeLabel :: String
                              , _nodeSubtree :: TreeDebugNode dom
                              }
                   | SimpleNode { _nodeLabel :: String
                                , _nodeValue :: String
                                }

deriving instance Domain dom => Show (DebugNode dom)

data TreeDebugNode dom
  = TreeDebugNode { _nodeName :: String
                  , _nodeInfo :: SemanticInfoType dom
                  , _children :: [DebugNode dom]
                  }

deriving instance Domain dom => Show (TreeDebugNode dom)

data SemanticInfoType dom 
  = DefaultInfoType (SemanticInfo' dom SameInfoDefaultCls)
  | NameInfoType (SemanticInfo' dom SameInfoNameCls)
  | ExprInfoType (SemanticInfo' dom SameInfoExprCls)
  | ImportInfoType (SemanticInfo' dom SameInfoImportCls)
  | ModuleInfoType (SemanticInfo' dom SameInfoModuleCls)

deriving instance Domain dom => Show (SemanticInfoType dom)

makeReferences ''DebugNode
makeReferences ''TreeDebugNode

--astDebug :: (InspectableName n, HasRange a, ASTDebug e a, a ~ (NodeInfo (SemanticInfo n) src)) => e a -> String
--astDebug ast = toList (astDebugToJson (astDebug' ast))

--astDebugToJson :: (InspectableName n, HasRange a, a ~ (NodeInfo (SemanticInfo n) src)) => [DebugNode a] -> Seq Char
--astDebugToJson nodes = fromList "[ " >< childrenJson >< fromList " ]"
--    where treeNodes = List.filter (\case TreeNode {} -> True; _ -> False) nodes
--          childrenJson = case map debugTreeNode treeNodes of 
--                           first:rest -> first >< foldl (><) Seq.empty (fmap (fromList ", " ><) (fromList rest))
--                           []         -> Seq.empty
--          debugTreeNode (TreeNode "" s) = astDebugElemJson s
--          debugTreeNode (TreeNode (dropWhile (=='_') -> l) s) = astDebugElemJson (nodeName .- (("<span class='astlab'>" ++ l ++ "</span>: ") ++) $ s)

--astDebugElemJson :: (InspectableName n, HasRange a, a ~ (NodeInfo (SemanticInfo n) src)) => TreeDebugNode a -> Seq Char
--astDebugElemJson (TreeDebugNode name info children) 
--  = fromList "{ \"text\" : \"" >< fromList name 
--     >< fromList "\", \"state\" : { \"opened\" : true }, \"a_attr\" : { \"data-range\" : \"" 
--     >< fromList (shortShowSpan (getRange info))
--     >< fromList "\", \"data-elems\" : \"" 
--     >< foldl (><) Seq.empty dataElems
--     >< fromList "\", \"data-sema\" : \"" 
--     >< fromList (showSema (info ^. semanticInfo))
--     >< fromList "\" }, \"children\" : " 
--     >< astDebugToJson children >< fromList " }"
--  where dataElems = catMaybes (map (\case SimpleNode l v -> Just (fromList (formatScalarElem l v)); _ -> Nothing) children)
--        formatScalarElem l v = "<div class='scalarelem'><span class='astlab'>" ++ l ++ "</span>: " ++ tail (init (show v)) ++ "</div>"
--        showSema info = "<div class='semaname'>" ++ assocName info ++ "</div>" 
--                          ++ concatMap (\(l,i) -> "<div class='scalarelem'><span class='astlab'>" ++ l ++ "</span>: " ++ i ++ "</div>") (toAssoc info) 

--class AssocData a where
--  assocName :: a -> String
--  toAssoc :: a -> [(String, String)]

--instance InspectableName n => AssocData (SemanticInfo (Dom2 n) e) where
--  assocName NoSemanticInfo = "NoSemanticInfo"
--  assocName (ScopeInfo {}) = "ScopeInfo"
--  assocName (NameInfo {}) = "NameInfo"
--  assocName (ModuleInfo {}) = "ModuleInfo"
--  assocName (ImportInfo {}) = "ImportInfo"

--  toAssoc NoSemanticInfo = []
--  toAssoc (ScopeInfo locals) = [ ("namesInScope", inspectScope locals) ]
--  toAssoc (NameInfo locals defined nameInfo) = [ ("name", inspect nameInfo)
--                                               , ("isDefined", show defined)
--                                               , ("namesInScope", inspectScope locals) 
--                                               ]
--  toAssoc (ModuleInfo mod imps) = [("moduleName", showSDocUnsafe (ppr mod))
--                                  ,("implicitImports", concat (intersperse ", " (map inspect imps)))]
--  toAssoc (ImportInfo mod avail imported) = [ ("moduleName", showSDocUnsafe (ppr mod)) 
--                                            , ("availableNames", concat (intersperse ", " (map inspect avail))) 
--                                            , ("importedNames", concat (intersperse ", " (map inspect imported))) 
--                                            ]

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

class (Domain dom, SourceInfo st) 
         => ASTDebug e dom st where
  astDebug' :: e dom st -> [DebugNode dom]
  default astDebug' :: (GAstDebug (Rep (e dom st)) dom, Generic (e dom st)) => e dom st -> [DebugNode dom]
  astDebug' = gAstDebug . from

class GAstDebug f dom where 
  gAstDebug :: f p -> [DebugNode dom]
  
instance GAstDebug V1 dom where
  gAstDebug _ = error "GAstDebug V1"
  
instance GAstDebug U1 dom where
  gAstDebug U1 = []  
  
instance (GAstDebug f dom, GAstDebug g dom) => GAstDebug (f :+: g) dom where
  gAstDebug (L1 x) = gAstDebug x
  gAstDebug (R1 x) = gAstDebug x
  
instance (GAstDebug f dom, GAstDebug g dom) => GAstDebug (f :*: g) dom where
  gAstDebug (x :*: y) 
    = gAstDebug x ++ gAstDebug y

instance {-# OVERLAPPING #-} ASTDebug e dom st => GAstDebug (K1 i (e dom st)) dom where
  gAstDebug (K1 x) = astDebug' x
  
instance {-# OVERLAPPABLE #-} Show x => GAstDebug (K1 i x) dom where
  gAstDebug (K1 x) = [SimpleNode "" (show x)]
        
instance (GAstDebug f dom, Constructor c) => GAstDebug (M1 C c f) dom where
  gAstDebug c@(M1 x) = [TreeNode "" (TreeDebugNode (conName c) undefined (gAstDebug x))]

instance (GAstDebug f dom, Selector s) => GAstDebug (M1 S s f) dom where
  gAstDebug s@(M1 x) = traversal&nodeLabel .= selName s $ gAstDebug x

-- don't have to do anything with datatype metainfo
instance GAstDebug f dom => GAstDebug (M1 D t f) dom where
  gAstDebug (M1 x) = gAstDebug x
