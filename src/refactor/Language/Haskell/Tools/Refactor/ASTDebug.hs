{-# LANGUAGE TypeOperators
           , DefaultSignatures
           , StandaloneDeriving
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , TemplateHaskell
           , OverloadedStrings
           , ConstraintKinds
           , LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           , UndecidableInstances
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
  = DefaultInfoType { semaInfoTypeRng :: SrcSpan 
                    }
  | NameInfoType { semaInfoTypeName :: SemanticInfo' dom SameInfoNameCls
                 , semaInfoTypeRng :: SrcSpan 
                 }
  | ExprInfoType { semaInfoTypeExpr :: SemanticInfo' dom SameInfoExprCls
                 , semaInfoTypeRng :: SrcSpan
                 }
  | ImportInfoType { semaInfoTypeImport :: SemanticInfo' dom SameInfoImportCls
                   , semaInfoTypeRng :: SrcSpan
                   }
  | ModuleInfoType { semaInfoTypeModule :: SemanticInfo' dom SameInfoModuleCls
                   , semaInfoTypeRng :: SrcSpan
                   }
  | ImplicitFieldInfoType { semaInfoTypeImplicitFld :: SemanticInfo' dom SameInfoWildcardCls
                          , semaInfoTypeRng :: SrcSpan
                          }


deriving instance Domain dom => Show (SemanticInfoType dom)

makeReferences ''DebugNode
makeReferences ''TreeDebugNode

type AssocSema dom = ( AssocData (SemanticInfo' dom SameInfoModuleCls), AssocData (SemanticInfo' dom SameInfoImportCls)
                     , AssocData (SemanticInfo' dom SameInfoNameCls), AssocData (SemanticInfo' dom SameInfoExprCls)
                     , AssocData (SemanticInfo' dom SameInfoWildcardCls) )

astDebug :: (ASTDebug e dom st, AssocSema dom) => e dom st -> String
astDebug ast = toList (astDebugToJson (astDebug' ast))

astDebugToJson :: AssocSema dom => [DebugNode dom] -> Seq Char
astDebugToJson nodes = fromList "[ " >< childrenJson >< fromList " ]"
    where treeNodes = List.filter (\case TreeNode {} -> True; _ -> False) nodes
          childrenJson = case map debugTreeNode treeNodes of 
                           first:rest -> first >< foldl (><) Seq.empty (fmap (fromList ", " ><) (fromList rest))
                           []         -> Seq.empty
          debugTreeNode (TreeNode "" s) = astDebugElemJson s
          debugTreeNode (TreeNode (dropWhile (=='_') -> l) s) = astDebugElemJson (nodeName .- (("<span class='astlab'>" ++ l ++ "</span>: ") ++) $ s)

astDebugElemJson :: AssocSema dom => TreeDebugNode dom -> Seq Char
astDebugElemJson (TreeDebugNode name info children) 
  = fromList "{ \"text\" : \"" >< fromList name 
     >< fromList "\", \"state\" : { \"opened\" : true }, \"a_attr\" : { \"data-range\" : \"" 
     >< fromList (shortShowSpan (semaInfoTypeRng info))
     >< fromList "\", \"data-elems\" : \"" 
     >< foldl (><) Seq.empty dataElems
     >< fromList "\", \"data-sema\" : \"" 
     >< fromList (showSema info)
     >< fromList "\" }, \"children\" : " 
     >< astDebugToJson children >< fromList " }"
  where dataElems = catMaybes (map (\case SimpleNode l v -> Just (fromList (formatScalarElem l v)); _ -> Nothing) children)
        formatScalarElem l v = "<div class='scalarelem'><span class='astlab'>" ++ l ++ "</span>: " ++ tail (init (show v)) ++ "</div>"
        showSema info = "<div class='semaname'>" ++ assocName info ++ "</div>" 
                          ++ concatMap (\(l,i) -> "<div class='scalarelem'><span class='astlab'>" ++ l ++ "</span>: " ++ i ++ "</div>") (toAssoc info) 

class AssocData a where
  assocName :: a -> String
  toAssoc :: a -> [(String, String)]

instance AssocSema dom => AssocData (SemanticInfoType dom) where
  assocName s@(DefaultInfoType {}) = "NoSemanticInfo"
  assocName s@(NameInfoType {}) = assocName (semaInfoTypeName s)
  assocName s@(ExprInfoType {}) = assocName (semaInfoTypeExpr s)
  assocName s@(ImportInfoType {}) = assocName (semaInfoTypeImport s)
  assocName s@(ModuleInfoType {}) = assocName (semaInfoTypeModule s)
  assocName s@(ImplicitFieldInfoType {}) = assocName (semaInfoTypeImplicitFld s)

  toAssoc s@(DefaultInfoType {}) = []
  toAssoc s@(NameInfoType {}) = toAssoc (semaInfoTypeName s)
  toAssoc s@(ExprInfoType {}) = toAssoc (semaInfoTypeExpr s)
  toAssoc s@(ImportInfoType {}) = toAssoc (semaInfoTypeImport s)
  toAssoc s@(ModuleInfoType {}) = toAssoc (semaInfoTypeModule s)
  toAssoc s@(ImplicitFieldInfoType {}) = toAssoc (semaInfoTypeImplicitFld s)


instance AssocData ScopeInfo where
  assocName (ScopeInfo {}) = "ScopeInfo"
  toAssoc (ScopeInfo locals) = [ ("namesInScope", inspectScope locals) ]

instance InspectableName n => AssocData (NameInfo n) where
  assocName (NameInfo {}) = "NameInfo"
  assocName (AmbiguousNameInfo {}) = "AmbiguousNameInfo"
  assocName (ImplicitNameInfo {}) = "ImplicitNameInfo"

  toAssoc (NameInfo locals defined nameInfo) = [ ("name", inspect nameInfo)
                                               , ("isDefined", show defined)
                                               , ("namesInScope", inspectScope locals) 
                                               ]
  toAssoc (AmbiguousNameInfo locals defined name _) = [ ("name", inspect name)
                                                      , ("isDefined", show defined)
                                                      , ("namesInScope", inspectScope locals) 
                                                      ]
  toAssoc (ImplicitNameInfo locals defined name _) = [ ("name", name)
                                                     , ("isDefined", show defined)
                                                     , ("namesInScope", inspectScope locals) 
                                                     ]
instance AssocData CNameInfo where
  assocName (CNameInfo {}) = "CNameInfo"
  toAssoc (CNameInfo locals defined nameInfo) = [ ("name", inspect nameInfo)
                                                , ("isDefined", show defined)
                                                , ("namesInScope", inspectScope locals) 
                                                ]

instance InspectableName n => AssocData (ModuleInfo n) where
  assocName (ModuleInfo {}) = "ModuleInfo"
  toAssoc (ModuleInfo mod isboot imps) = [ ("moduleName", showSDocUnsafe (ppr mod))
                                         , ("isBoot", show isboot)
                                         , ("implicitImports", concat (intersperse ", " (map inspect imps)))
                                         ]
  
instance InspectableName n => AssocData (ImportInfo n) where
  assocName (ImportInfo {}) = "ImportInfo"
  toAssoc (ImportInfo mod avail imported) = [ ("moduleName", showSDocUnsafe (ppr mod)) 
                                            , ("availableNames", concat (intersperse ", " (map inspect avail))) 
                                            , ("importedNames", concat (intersperse ", " (map inspect imported))) 
                                            ]      
  
instance AssocData ImplicitFieldInfo where
  assocName (ImplicitFieldInfo {}) = "ImplicitFieldInfo"
  toAssoc (ImplicitFieldInfo bnds) = [ ("bindings", concat (intersperse ", " (map (\(from,to) -> "(" ++ inspect from ++ " -> " ++ inspect to ++ ")") bnds)))
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
