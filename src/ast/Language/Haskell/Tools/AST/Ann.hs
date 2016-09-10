{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , TemplateHaskell
           , DeriveDataTypeable
           , StandaloneDeriving
           , KindSignatures
           , TypeFamilies
           , MultiParamTypeClasses
           , UndecidableInstances
           , AllowAmbiguousTypes
           , TypeApplications
           , ScopedTypeVariables
           #-}
-- | Parts of AST representation for keeping extra data
module Language.Haskell.Tools.AST.Ann where

import Data.Data
import Control.Monad.Identity
import Control.Reference
import SrcLoc as GHC
import Id as GHC
import qualified Name as GHC
import RdrName
import Module
import Id
import Outputable
import Language.Haskell.Tools.AST.Utils.GHCInstances
import Language.Haskell.Tools.AST.SemaInfoTypes

import {-# SOURCE #-} Language.Haskell.Tools.AST.Modules as AST
import {-# SOURCE #-} Language.Haskell.Tools.AST.Base as AST
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs as AST

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
  SemaInfoClassify QualifiedName    = SameInfoNameCls
  SemaInfoClassify Expr          = SameInfoExprCls
  SemaInfoClassify ImportDecl    = SameInfoImportCls
  SemaInfoClassify AST.Module    = SameInfoModuleCls
  SemaInfoClassify FieldWildcard = SameInfoWildcardCls
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

-- | Class for domain configuration markers
class ( Typeable d
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
      ) => Domain d where

-- | A semantic domain for the AST. The semantic domain maps semantic information for
-- the different types of nodes in the AST. The kind of semantic domain for an AST
-- depends on which stages of the compilation did it pass. However after transforming
-- the GHC representation to our AST, the domain keeps the same.
-- The domain is not applied to the AST elements that are generated while refactoring.
instance ( Typeable d
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
         ) => Domain d where


class ( Data (SemanticInfo' d (SemaInfoClassify e))
      , Show (SemanticInfo' d (SemaInfoClassify e))
      , Domain d
      ) => DomainWith e d where

instance ( Data (SemanticInfo' d (SemaInfoClassify e))
         , Show (SemanticInfo' d (SemaInfoClassify e))
         , Domain d
         ) => DomainWith e d where

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
      ) 
         => SourceInfo stage where
  -- | Type of source info for normal AST elements
  data SpanInfo stage :: *
  -- | Type of source info for lists of AST elements
  data ListInfo stage :: *
  -- | Type of source info for optional AST elements
  data OptionalInfo stage :: *


instance SourceInfo RangeStage where
  data SpanInfo RangeStage = NodeSpan { _nodeSpan :: SrcSpan }
    deriving (Data)
  data ListInfo RangeStage = ListPos  { _listBefore :: String
                                      , _listAfter :: String
                                      , _listDefaultSep :: String
                                      , _listIndented :: Bool
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
                                              , _normListIndented :: Bool
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
data AnnList elem dom stage = AnnList { _annListAnnot :: NodeInfo (SemanticInfo dom (AnnList elem)) (ListInfo stage) 
                                      , _annListElems :: [Ann elem dom stage]
                                      }
                           
makeReferences ''AnnList
        
annList :: Traversal (AnnList e d s) (AnnList e d s) (Ann e d s) (Ann e d s)                          
annList = annListElems & traversal

-- | An optional AST element
data AnnMaybe elem dom stage = AnnMaybe { _annMaybeAnnot :: NodeInfo (SemanticInfo dom (AnnMaybe elem)) (OptionalInfo stage)
                                        , _annMaybe :: Maybe (Ann elem dom stage)
                                        }
                             
makeReferences ''AnnMaybe
                          
annJust :: Partial (AnnMaybe e d s) (AnnMaybe e d s) (Ann e d s) (Ann e d s)                          
annJust = annMaybe & just

-- | An empty list of AST elements
annNil :: NodeInfo (SemanticInfo d (AnnList e)) (ListInfo s) -> AnnList e d s
annNil a = AnnList a []

isAnnNothing :: AnnMaybe e d s -> Bool
isAnnNothing (AnnMaybe _ Nothing) = True
isAnnNothing (AnnMaybe _ _) = False

isAnnJust :: AnnMaybe e d s -> Bool
isAnnJust (AnnMaybe _ (Just _)) = True
isAnnJust (AnnMaybe _ _) = False

-- | A non-existing AST part
annNothing :: NodeInfo (SemanticInfo d (AnnMaybe e)) (OptionalInfo s) -> AnnMaybe e d s
annNothing a = AnnMaybe a Nothing


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

instance SourceInfo stage => HasRange (AnnList elem dom stage) where
  getRange (AnnList a _) = getRange (a ^. sourceInfo)
  setRange sp = annListAnnot & sourceInfo .- setRange sp

instance SourceInfo stage => HasRange (AnnMaybe elem dom stage) where
  getRange (AnnMaybe a _) = getRange (a ^. sourceInfo)
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

instance (ApplySemaChange (SemaInfoClassify e), SemanticTraversal e) => SemanticTraversal (AnnList e) where
  semaTraverse f (AnnList (NodeInfo sema src) e) = AnnList <$> (NodeInfo <$> trfSemaDefault f sema <*> pure src) <*> mapM (semaTraverse f) e

instance (ApplySemaChange (SemaInfoClassify e), SemanticTraversal e) => SemanticTraversal (AnnMaybe e) where
  semaTraverse f (AnnMaybe (NodeInfo sema src) e) = AnnMaybe <$> (NodeInfo <$> trfSemaDefault f sema <*> pure src) <*> sequence (fmap (semaTraverse f) e)

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

instance SourceInfoTraversal e => SourceInfoTraversal (AnnList e) where
  sourceInfoTraverse trf (AnnList (NodeInfo sema src) e) 
    = AnnList <$> (NodeInfo sema <$> trfListInfo trf src) <*> mapM (sourceInfoTraverse trf) e
  sourceInfoTraverseDown trf desc asc (AnnList (NodeInfo sema src) e) 
    = AnnList <$> (NodeInfo sema <$> trfListInfo trf src) <*> (desc *> mapM (sourceInfoTraverseDown trf desc asc) e <* asc)
  sourceInfoTraverseUp trf desc asc (AnnList (NodeInfo sema src) e) 
    = flip AnnList <$> (desc *> mapM (sourceInfoTraverseUp trf desc asc) e <* asc) <*> (NodeInfo sema <$> trfListInfo trf src)

instance SourceInfoTraversal e => SourceInfoTraversal (AnnMaybe e) where
  sourceInfoTraverse trf (AnnMaybe (NodeInfo sema src) e) 
    = AnnMaybe <$> (NodeInfo sema <$> trfOptionalInfo trf src) <*> sequence (fmap (sourceInfoTraverse trf) e)
  sourceInfoTraverseDown trf desc asc (AnnMaybe (NodeInfo sema src) e) 
    = AnnMaybe <$> (NodeInfo sema <$> trfOptionalInfo trf src) <*> (desc *> sequence (fmap (sourceInfoTraverseDown trf desc asc) e) <* asc)
  sourceInfoTraverseUp trf desc asc (AnnMaybe (NodeInfo sema src) e) 
    = flip AnnMaybe <$> (desc *> sequence (fmap (sourceInfoTraverseUp trf desc asc) e) <* asc) <*> (NodeInfo sema <$> trfOptionalInfo trf src)

