{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeApplications, TypeFamilies, UndecidableInstances #-}
-- | Parts of AST representation for keeping extra data
module Language.Haskell.Tools.AST.Ann where

import Control.Reference
import Data.Data
import FastString
import Id as GHC
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.AST.Utils.GHCInstances ()
import qualified Name as GHC
import SrcLoc as GHC

import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Exprs as AST
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Modules as AST
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Names as AST

-- * Annotation type resolution

-- | A stage in which the nodes are marked with the ranges in the source
-- files which contain the source code of the given AST element.
data RangeStage

deriving instance Data RangeStage

-- | A stage in which the nodes are still marked with ranges, but these
-- ranges are normalized. Optional and list elements also have ranges
-- in that state.
data NormRangeStage

deriving instance Data NormRangeStage

-- | A stage in which AST elements are marked with templates. These
-- templates are hierarchical, and contain the places of the children
-- elements of the node.
data RngTemplateStage

deriving instance Data RngTemplateStage

-- | A stage where the annotation controls how the original source code can be
-- retrieved from the AST. A source template is assigned to each node.
-- It has holes where the content of an other node should be printed and
-- ranges for the source code of the node.
data SrcTemplateStage

deriving instance Data SrcTemplateStage

-- | With this domain, semantic information can be parameterized. In practice
-- it is only used if the compilation cannot proceed past the type checking phase.
data Dom name

-- Semantic information that contains types. Only normal names remain in this domain.
data IdDom

deriving instance (Data name, Typeable name) => Data (Dom name)

deriving instance Data IdDom

type SemanticInfo (domain :: *) (node :: * -> * -> *) = SemanticInfo' domain (SemaInfoClassify node)

data SameInfoNameCls
data SameInfoExprCls
data SameInfoImportCls
data SameInfoModuleCls
data SameInfoDefaultCls
data SameInfoWildcardCls

type family SemaInfoClassify (node :: * -> * -> *) where
  SemaInfoClassify UQualifiedName = SameInfoNameCls
  SemaInfoClassify UExpr          = SameInfoExprCls
  SemaInfoClassify UImportDecl    = SameInfoImportCls
  SemaInfoClassify AST.UModule    = SameInfoModuleCls
  SemaInfoClassify UFieldWildcard = SameInfoWildcardCls
  SemaInfoClassify a             = SameInfoDefaultCls

type family SemanticInfo' (domain :: *) (nodecls :: *)

type instance SemanticInfo' (Dom n) SameInfoNameCls = NameInfo n
type instance SemanticInfo' (Dom n) SameInfoExprCls = ScopeInfo
type instance SemanticInfo' (Dom n) SameInfoImportCls = ImportInfo n
type instance SemanticInfo' (Dom n) SameInfoModuleCls = ModuleInfo GHC.Name
type instance SemanticInfo' (Dom n) SameInfoWildcardCls = ImplicitFieldInfo
type instance SemanticInfo' (Dom n) SameInfoDefaultCls = NoSemanticInfo

type instance SemanticInfo' IdDom SameInfoNameCls = CNameInfo
type instance SemanticInfo' IdDom SameInfoExprCls = ScopeInfo
type instance SemanticInfo' IdDom SameInfoImportCls = ImportInfo GHC.Id
type instance SemanticInfo' IdDom SameInfoModuleCls = ModuleInfo GHC.Id
type instance SemanticInfo' IdDom SameInfoWildcardCls = ImplicitFieldInfo
type instance SemanticInfo' IdDom SameInfoDefaultCls = NoSemanticInfo

-- | A semantic domain for the AST. The semantic domain maps semantic information for
-- the different types of nodes in the AST. The kind of semantic domain for an AST
-- depends on which stages of the compilation did it pass. However after transforming
-- the GHC representation to our AST, the domain keeps the same.
-- The domain is not applied to the AST elements that are generated while refactoring.
type Domain d = ( Typeable d
                , Data d
                , SemanticInfo' d SameInfoDefaultCls ~ NoSemanticInfo
                , Data (SemanticInfo' d SameInfoNameCls)
                , Data (SemanticInfo' d SameInfoExprCls)
                , Data (SemanticInfo' d SameInfoImportCls)
                , Data (SemanticInfo' d SameInfoModuleCls)
                , Data (SemanticInfo' d SameInfoWildcardCls)
                , Show (SemanticInfo' d SameInfoNameCls)
                , Show (SemanticInfo' d SameInfoExprCls)
                , Show (SemanticInfo' d SameInfoImportCls)
                , Show (SemanticInfo' d SameInfoModuleCls)
                , Show (SemanticInfo' d SameInfoWildcardCls)
                )

type DomainWith e d = ( Data (SemanticInfo' d (SemaInfoClassify e))
                      , Show (SemanticInfo' d (SemaInfoClassify e))
                      , Domain d
                      )

-- | Extracts or modifies the concrete range corresponding to a given source info.
-- In case of lists and optional elements, it may not contain the elements inside.
class HasRange a where
  getRange :: a -> SrcSpan
  setRange :: SrcSpan -> a -> a

-- | Class for source information stages
class ( Typeable stage
      , Data stage
      , Data (SpanInfo stage)
      , Data (ListInfo stage)
      , Data (OptionalInfo stage)
      , Show (SpanInfo stage)
      , Show (ListInfo stage)
      , Show (OptionalInfo stage)
      , HasRange (SpanInfo stage)
      , HasRange (ListInfo stage)
      , HasRange (OptionalInfo stage)
      ) => SourceInfo stage where

  -- | UType of source info for normal AST elements
  data SpanInfo stage :: *
  -- | UType of source info for lists of AST elements
  data ListInfo stage :: *
  -- | UType of source info for optional AST elements
  data OptionalInfo stage :: *


instance SourceInfo RangeStage where
  data SpanInfo RangeStage = NodeSpan { _nodeSpan :: SrcSpan }
    deriving (Data)
  data ListInfo RangeStage = ListPos  { _listBefore :: String
                                      , _listAfter :: String
                                      , _listDefaultSep :: String
                                      , _listIndented :: Maybe [Bool]
                                      , _listPos :: SrcLoc
                                      }
    deriving (Data)
  data OptionalInfo RangeStage = OptionalPos { _optionalBefore :: String
                                             , _optionalAfter :: String
                                             , _optionalPos :: SrcLoc
                                             }
    deriving (Data)

instance Show (SpanInfo RangeStage) where
  show (NodeSpan sp) = shortShowSpan sp

instance Show (ListInfo RangeStage) where
  show sp = shortShowLoc (_listPos sp)

instance Show (OptionalInfo RangeStage) where
  show sp = shortShowLoc (_optionalPos sp)

instance SourceInfo NormRangeStage where
  data SpanInfo NormRangeStage = NormNodeInfo { _normNodeSpan :: SrcSpan }
    deriving (Data)
  data ListInfo NormRangeStage = NormListInfo { _normListBefore :: String
                                              , _normListAfter :: String
                                              , _normListDefaultSep :: String
                                              , _normListIndented :: Maybe [Bool]
                                              , _normListSpan :: SrcSpan
                                              }
    deriving (Data)
  data OptionalInfo NormRangeStage = NormOptInfo { _normOptBefore :: String
                                                 , _normOptAfter :: String
                                                 , _normOptSpan :: SrcSpan
                                                 }
    deriving (Data)

instance Show (SpanInfo NormRangeStage) where
  show (NormNodeInfo sp) = shortShowSpan sp

instance Show (ListInfo NormRangeStage) where
  show sp = shortShowSpan (_normListSpan sp)

instance Show (OptionalInfo NormRangeStage) where
  show sp = shortShowSpan (_normOptSpan sp)

-- | A short form of showing a range, without file name, for debugging purposes.
shortShowSpan :: SrcSpan -> String
shortShowSpan (UnhelpfulSpan _) = "??-??"
shortShowSpan sp@(RealSrcSpan _)
  = shortShowLoc (srcSpanStart sp) ++ "-" ++ shortShowLoc (srcSpanEnd sp)

shortShowSpanWithFile :: SrcSpan -> String
shortShowSpanWithFile (UnhelpfulSpan _) = "?? ??-??"
shortShowSpanWithFile sp@(RealSrcSpan rsp)
  = unpackFS (srcSpanFile rsp) ++ " " ++ shortShowLoc (srcSpanStart sp) ++ "-" ++ shortShowLoc (srcSpanEnd sp)

-- | A short form of showing a range, without file name, for debugging purposes.
shortShowLoc :: SrcLoc -> String
shortShowLoc (UnhelpfulLoc _) = "??"
shortShowLoc (RealSrcLoc loc) = show (srcLocLine loc) ++ ":" ++ show (srcLocCol loc)

-- | A class for marking a source information stage. All programs, regardless of
-- correct Haskell programs or not, must go through these stages to be refactored.
class SourceInfo stage
   => RangeInfo stage where
  nodeSpan :: Simple Lens (SpanInfo stage) GHC.SrcSpan
  listPos :: Simple Lens (ListInfo stage) GHC.SrcLoc
  optionalPos :: Simple Lens (OptionalInfo stage) GHC.SrcLoc

instance RangeInfo RangeStage where
  nodeSpan = lens _nodeSpan (\v s -> s { _nodeSpan = v })
  listPos = lens _listPos (\v s -> s { _listPos = v })
  optionalPos = lens _optionalPos (\v s -> s { _optionalPos = v })

-- * Annotations

-- | Semantic and source code related information for an AST node.
data NodeInfo sema src
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }
  deriving (Eq, Show, Data)

makeReferences ''NodeInfo

-- | An element of the AST keeping extra information.
data Ann elem dom stage
-- The type parameters are organized this way because we want the annotation type to
-- be more flexible, but the annotation is the first parameter because it eases
-- pattern matching.
  = Ann { _annotation :: NodeInfo (SemanticInfo dom elem) (SpanInfo stage) -- ^ The extra information for the AST part
        , _element    :: elem dom stage -- ^ The original AST part
        }

makeReferences ''Ann

-- | A list of AST elements
data AnnListG elem dom stage = AnnListG { _annListAnnot :: NodeInfo (SemanticInfo dom (AnnListG elem)) (ListInfo stage)
                                        , _annListElems :: [Ann elem dom stage]
                                        }

makeReferences ''AnnListG

annList :: Traversal (AnnListG e d s) (AnnListG e d s) (Ann e d s) (Ann e d s)
annList = annListElems & traversal

-- | An optional AST element
data AnnMaybeG elem dom stage = AnnMaybeG { _annMaybeAnnot :: NodeInfo (SemanticInfo dom (AnnMaybeG elem)) (OptionalInfo stage)
                                          , _annMaybe :: Maybe (Ann elem dom stage)
                                          }

makeReferences ''AnnMaybeG

class HasSourceInfo e where
  type SourceInfoType e :: *
  srcInfo :: Simple Lens e (SourceInfoType e)

instance HasSourceInfo (Ann elem dom stage) where
  type SourceInfoType (Ann elem dom stage) = SpanInfo stage
  srcInfo = annotation & sourceInfo

instance HasSourceInfo (AnnListG elem dom stage) where
  type SourceInfoType (AnnListG elem dom stage) = ListInfo stage
  srcInfo = annListAnnot & sourceInfo

instance HasSourceInfo (AnnMaybeG elem dom stage) where
  type SourceInfoType (AnnMaybeG elem dom stage) = OptionalInfo stage
  srcInfo = annMaybeAnnot & sourceInfo

annJust :: Partial (AnnMaybeG e d s) (AnnMaybeG e d s) (Ann e d s) (Ann e d s)
annJust = annMaybe & just

-- | An empty list of AST elements
annNil :: NodeInfo (SemanticInfo d (AnnListG e)) (ListInfo s) -> AnnListG e d s
annNil a = AnnListG a []

isAnnNothing :: AnnMaybeG e d s -> Bool
isAnnNothing (AnnMaybeG _ Nothing) = True
isAnnNothing (AnnMaybeG _ _) = False

isAnnJust :: AnnMaybeG e d s -> Bool
isAnnJust (AnnMaybeG _ (Just _)) = True
isAnnJust (AnnMaybeG _ _) = False

annLength :: AnnListG e d s -> Int
annLength (AnnListG _ ls) = length ls

-- | A non-existing AST part
annNothing :: NodeInfo (SemanticInfo d (AnnMaybeG e)) (OptionalInfo s) -> AnnMaybeG e d s
annNothing a = AnnMaybeG a Nothing

-- * Info types

instance HasRange (SpanInfo RangeStage) where
  getRange (NodeSpan sp) = sp
  setRange sp (NodeSpan _) = NodeSpan sp

instance HasRange (ListInfo RangeStage) where
  getRange ListPos{_listPos = pos} = srcLocSpan pos
  setRange sp info = info {_listPos = srcSpanStart sp}

instance HasRange (OptionalInfo RangeStage) where
  getRange OptionalPos{_optionalPos = pos} = srcLocSpan pos
  setRange sp info = info {_optionalPos = srcSpanStart sp}

instance HasRange (SpanInfo NormRangeStage) where
  getRange (NormNodeInfo sp) = sp
  setRange sp (NormNodeInfo _) = NormNodeInfo sp

instance HasRange (ListInfo NormRangeStage) where
  getRange NormListInfo{_normListSpan = sp} = sp
  setRange sp info = info { _normListSpan = sp }

instance HasRange (OptionalInfo NormRangeStage) where
  getRange NormOptInfo{_normOptSpan = sp} = sp
  setRange sp info = info {_normOptSpan = sp}

instance SourceInfo stage => HasRange (Ann elem dom stage) where
  getRange (Ann a _) = getRange (a ^. sourceInfo)
  setRange sp = annotation & sourceInfo .- setRange sp

instance SourceInfo stage => HasRange (AnnListG elem dom stage) where
  getRange (AnnListG a _) = getRange (a ^. sourceInfo)
  setRange sp = annListAnnot & sourceInfo .- setRange sp

instance SourceInfo stage => HasRange (AnnMaybeG elem dom stage) where
  getRange (AnnMaybeG a _) = getRange (a ^. sourceInfo)
  setRange sp = annMaybeAnnot & sourceInfo .- setRange sp

-- | A class for changing semantic information throught the AST.
class ApplySemaChange cls where
  appSemaChange :: SemaTrf f dom1 dom2 -> SemanticInfo' dom1 cls -> f (SemanticInfo' dom2 cls)

instance ApplySemaChange SameInfoNameCls where appSemaChange = trfSemaNameCls
instance ApplySemaChange SameInfoExprCls where appSemaChange = trfSemaExprCls
instance ApplySemaChange SameInfoImportCls where appSemaChange = trfSemaImportCls
instance ApplySemaChange SameInfoModuleCls where appSemaChange = trfSemaModuleCls
instance ApplySemaChange SameInfoWildcardCls where appSemaChange = trfSemaWildcardCls
instance ApplySemaChange SameInfoDefaultCls where appSemaChange = trfSemaDefault

-- | A class for traversing semantic information in an AST
class ApplySemaChange (SemaInfoClassify a)
   => SemanticTraversal a where
  semaTraverse :: Monad f => SemaTrf f dom1 dom2 -> a dom1 st -> f (a dom2 st)

-- | A transformation on the possible semantic informations for a given domain
data SemaTrf f dom1 dom2 = SemaTrf { trfSemaNameCls :: SemanticInfo' dom1 SameInfoNameCls -> f (SemanticInfo' dom2 SameInfoNameCls)
                                   , trfSemaExprCls :: SemanticInfo' dom1 SameInfoExprCls -> f (SemanticInfo' dom2 SameInfoExprCls)
                                   , trfSemaImportCls :: SemanticInfo' dom1 SameInfoImportCls -> f (SemanticInfo' dom2 SameInfoImportCls)
                                   , trfSemaModuleCls :: SemanticInfo' dom1 SameInfoModuleCls -> f (SemanticInfo' dom2 SameInfoModuleCls)
                                   , trfSemaWildcardCls :: SemanticInfo' dom1 SameInfoWildcardCls -> f (SemanticInfo' dom2 SameInfoWildcardCls)
                                   , trfSemaDefault :: SemanticInfo' dom1 SameInfoDefaultCls -> f (SemanticInfo' dom2 SameInfoDefaultCls)
                                   }

instance forall e . (ApplySemaChange (SemaInfoClassify e), SemanticTraversal e) => SemanticTraversal (Ann e) where
  semaTraverse f (Ann (NodeInfo sema src) e) = Ann <$> (NodeInfo <$> appSemaChange @(SemaInfoClassify e) f sema <*> pure src) <*> semaTraverse f e

instance (ApplySemaChange (SemaInfoClassify e), SemanticTraversal e) => SemanticTraversal (AnnListG e) where
  semaTraverse f (AnnListG (NodeInfo sema src) e) = AnnListG <$> (NodeInfo <$> trfSemaDefault f sema <*> pure src) <*> mapM (semaTraverse f) e

instance (ApplySemaChange (SemaInfoClassify e), SemanticTraversal e) => SemanticTraversal (AnnMaybeG e) where
  semaTraverse f (AnnMaybeG (NodeInfo sema src) e) = AnnMaybeG <$> (NodeInfo <$> trfSemaDefault f sema <*> pure src) <*> sequence (fmap (semaTraverse f) e)

-- | A class for traversing source information in an AST
class SourceInfoTraversal a where
  sourceInfoTraverseUp :: Monad f => SourceInfoTrf f st1 st2 -> f () -> f () -> a dom st1 -> f (a dom st2)
  sourceInfoTraverseDown :: Monad f => SourceInfoTrf f st1 st2 -> f () -> f () -> a dom st1 -> f (a dom st2)
  sourceInfoTraverse :: Monad f => SourceInfoTrf f st1 st2 -> a dom st1 -> f (a dom st2)

-- | A transformation on the possible source informations
data SourceInfoTrf f st1 st2 = SourceInfoTrf { trfSpanInfo :: SpanInfo st1 -> f (SpanInfo st2)
                                             , trfListInfo :: ListInfo st1 -> f (ListInfo st2)
                                             , trfOptionalInfo :: OptionalInfo st1 -> f (OptionalInfo st2)
                                             }

instance SourceInfoTraversal e => SourceInfoTraversal (Ann e) where
  sourceInfoTraverse trf (Ann (NodeInfo sema src) e)
    = Ann <$> (NodeInfo sema <$> trfSpanInfo trf src) <*> sourceInfoTraverse trf e
  sourceInfoTraverseDown trf desc asc (Ann (NodeInfo sema src) e)
    = Ann <$> (NodeInfo sema <$> trfSpanInfo trf src) <*> (desc *> sourceInfoTraverseDown trf desc asc e <* asc)
  sourceInfoTraverseUp trf desc asc (Ann (NodeInfo sema src) e)
    = flip Ann <$> (desc *> sourceInfoTraverseUp trf desc asc e <* asc) <*> (NodeInfo sema <$> trfSpanInfo trf src)

instance SourceInfoTraversal e => SourceInfoTraversal (AnnListG e) where
  sourceInfoTraverse trf (AnnListG (NodeInfo sema src) e)
    = AnnListG <$> (NodeInfo sema <$> trfListInfo trf src) <*> mapM (sourceInfoTraverse trf) e
  sourceInfoTraverseDown trf desc asc (AnnListG (NodeInfo sema src) e)
    = AnnListG <$> (NodeInfo sema <$> trfListInfo trf src) <*> (desc *> mapM (sourceInfoTraverseDown trf desc asc) e <* asc)
  sourceInfoTraverseUp trf desc asc (AnnListG (NodeInfo sema src) e)
    = flip AnnListG <$> (desc *> mapM (sourceInfoTraverseUp trf desc asc) e <* asc) <*> (NodeInfo sema <$> trfListInfo trf src)

instance SourceInfoTraversal e => SourceInfoTraversal (AnnMaybeG e) where
  sourceInfoTraverse trf (AnnMaybeG (NodeInfo sema src) e)
    = AnnMaybeG <$> (NodeInfo sema <$> trfOptionalInfo trf src) <*> sequence (fmap (sourceInfoTraverse trf) e)
  sourceInfoTraverseDown trf desc asc (AnnMaybeG (NodeInfo sema src) e)
    = AnnMaybeG <$> (NodeInfo sema <$> trfOptionalInfo trf src) <*> (desc *> sequence (fmap (sourceInfoTraverseDown trf desc asc) e) <* asc)
  sourceInfoTraverseUp trf desc asc (AnnMaybeG (NodeInfo sema src) e)
    = flip AnnMaybeG <$> (desc *> sequence (fmap (sourceInfoTraverseUp trf desc asc) e) <* asc) <*> (NodeInfo sema <$> trfOptionalInfo trf src)
