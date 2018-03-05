{-# LANGUAGE TypeOperators, DefaultSignatures, StandaloneDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TemplateHaskell, OverloadedStrings, ConstraintKinds, LambdaCase, ViewPatterns, ScopedTypeVariables, UndecidableInstances #-}

-- | A module for displaying the AST in a tree view.
module Language.Haskell.Tools.ASTDebug where

import Control.Reference
import Data.Foldable
import Data.List as List
import Data.Maybe
import Data.Sequence as Seq (Seq, (><), empty, fromList)
import GHC.Generics
import Outputable
import SrcLoc

import DynFlags as GHC
import Id as GHC
import Name as GHC
import RdrName as GHC
import Type as GHC
import Unique as GHC

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.SemaInfoTypes


data DebugNode dom = TreeNode { _nodeLabel :: String
                              , _nodeSubtree :: TreeDebugNode dom
                              }
                   | SimpleNode { _nodeLabel :: String
                                , _nodeValue :: String
                                }

-- deriving instance Domain dom => Show (DebugNode dom)

data TreeDebugNode dom
  = TreeDebugNode { _nodeName :: String
                  , _nodeInfo :: SemanticInfoType dom
                  , _children :: [DebugNode dom]
                  }

-- deriving instance Domain dom => Show (TreeDebugNode dom)

data SemanticInfoType dom
  = DefaultInfoType { semaInfoTypeRng :: SrcSpan
                    }
  | NameInfoType { semaInfoTypeName :: SemanticInfo' dom SemaInfoNameCls
                 , semaInfoTypeRng :: SrcSpan
                 }
  | ExprInfoType { semaInfoTypeExpr :: SemanticInfo' dom SemaInfoExprCls
                 , semaInfoTypeRng :: SrcSpan
                 }
  | ImportInfoType { semaInfoTypeImport :: SemanticInfo' dom SemaInfoImportCls
                   , semaInfoTypeRng :: SrcSpan
                   }
  | ModuleInfoType { semaInfoTypeModule :: SemanticInfo' dom SemaInfoModuleCls
                   , semaInfoTypeRng :: SrcSpan
                   }
  | ImplicitFieldInfoType { semaInfoTypeImplicitFld :: SemanticInfo' dom SemaInfoWildcardCls
                          , semaInfoTypeRng :: SrcSpan
                          }

makeReferences ''DebugNode
makeReferences ''TreeDebugNode

type AssocSema dom = ( AssocData (SemanticInfo' dom SemaInfoModuleCls), AssocData (SemanticInfo' dom SemaInfoImportCls)
                     , AssocData (SemanticInfo' dom SemaInfoNameCls), AssocData (SemanticInfo' dom SemaInfoExprCls)
                     , AssocData (SemanticInfo' dom SemaInfoWildcardCls) )

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
          debugTreeNode (SimpleNode {}) = error "debugTreeNode: simple SimpleNode not allowed"

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
  assocName (DefaultInfoType {}) = "NoSemanticInfo"
  assocName s@(NameInfoType {}) = assocName (semaInfoTypeName s)
  assocName s@(ExprInfoType {}) = assocName (semaInfoTypeExpr s)
  assocName s@(ImportInfoType {}) = assocName (semaInfoTypeImport s)
  assocName s@(ModuleInfoType {}) = assocName (semaInfoTypeModule s)
  assocName s@(ImplicitFieldInfoType {}) = assocName (semaInfoTypeImplicitFld s)

  toAssoc (DefaultInfoType {}) = []
  toAssoc s@(NameInfoType {}) = toAssoc (semaInfoTypeName s)
  toAssoc s@(ExprInfoType {}) = toAssoc (semaInfoTypeExpr s)
  toAssoc s@(ImportInfoType {}) = toAssoc (semaInfoTypeImport s)
  toAssoc s@(ModuleInfoType {}) = toAssoc (semaInfoTypeModule s)
  toAssoc s@(ImplicitFieldInfoType {}) = toAssoc (semaInfoTypeImplicitFld s)


instance AssocData ScopeInfo where
  assocName _ = "ScopeInfo"
  toAssoc si = [ ("namesInScope", inspectScope (semanticsScope si)) ]

instance HasNameInfo' (NameInfo n) => AssocData (NameInfo n) where
  assocName _ = "NameInfo"

  toAssoc ni = [ ("name", maybe "<ambiguous>" inspect (semanticsName ni))
               , ("isDefined", show (semanticsDefining ni))
               , ("namesInScope", inspectScope (semanticsScope ni))
               ]

instance AssocData CNameInfo where
  assocName _ = "CNameInfo"
  toAssoc ni = [ ("name", inspect (semanticsId ni))
               , ("isDefined", show (semanticsDefining ni))
               , ("fixity", maybe "" (showSDocUnsafe . ppr) (semanticsFixity ni))
               , ("namesInScope", inspectScope (semanticsScope ni))
               ]

instance (HasModuleInfo' (ModuleInfo n)) => AssocData (ModuleInfo n) where
  assocName _ = "ModuleInfo"
  toAssoc mi = [ ("moduleName", showSDocUnsafe (ppr (semanticsModule mi)))
               , ("isBoot", show (isBootModule mi))
               , ("implicitImports", concat (intersperse ", " (map inspect (semanticsImplicitImports mi))))
               ]

instance (HasImportInfo' (ImportInfo n)) => AssocData (ImportInfo n) where
  assocName _ = "ImportInfo"
  toAssoc ii = [ ("moduleName", showSDocUnsafe (ppr (semanticsImportedModule ii)))
               , ("availableNames", concat (intersperse ", " (map inspect (semanticsAvailable ii))))
               , ("importedNames", concat (intersperse ", " (map inspect (semanticsImported ii))))
               ]

instance AssocData ImplicitFieldInfo where
  assocName _ = "ImplicitFieldInfo"
  toAssoc ifi = [ ("bindings", concat (intersperse ", " (map (\(from,to) -> "(" ++ inspect from ++ " -> " ++ inspect to ++ ")") (semanticsImplicitFlds ifi))))
                ]

inspectScope :: InspectableName n => [[(n, Maybe [UsageSpec], Maybe n)]] -> String
inspectScope = concat . intersperse " | " . map (concat . intersperse ", " . map showUsage)

class Outputable n => InspectableName n where
  inspect :: n -> String

showUsage :: InspectableName n => (n, Maybe [UsageSpec], Maybe n) -> String
showUsage (n,usage,parent) = inspect n ++ show usage ++ maybe "" (\p -> "( in " ++ showSDocUnsafe (ppr p) ++ ")") parent

instance Show UsageSpec where
  show (UsageSpec q useQ asQ)
    = (if q then "qualified " else "") ++ "as " ++ (if useQ == asQ || q then asQ else asQ ++ " or " ++ useQ)

instance InspectableName GHC.Name where
  inspect name = showSDocUnsafe (ppr name) ++ "[" ++ show (getUnique name) ++ "]"

instance InspectableName GHC.RdrName where
  inspect name = showSDocUnsafe (ppr name)

instance InspectableName GHC.Id where
  inspect name = showSDocUnsafe (ppr name) ++ "[" ++ show (getUnique name) ++ "] :: " ++ showSDocOneLine unsafeGlobalDynFlags (ppr (idType name)) ++ " (" ++ addendum ++ ")"
    where addendum = concat $ intersperse "," $ map (\v -> showSDocUnsafe (ppr v) ++ "[" ++ show (getUnique v) ++ "]") (getTVs (idType name))

getTVs :: GHC.Type -> [GHC.Var]
getTVs t
  | Just tv <- getTyVar_maybe t = [tv]
  | Just (op, arg) <- splitAppTy_maybe t = getTVs op `union` getTVs arg
  | Just (_, t') <- splitForAllTy_maybe t = getTVs t'
  | otherwise = []

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
