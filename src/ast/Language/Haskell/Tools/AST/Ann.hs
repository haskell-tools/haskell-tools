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

data RangeStage

deriving instance Data RangeStage

data  RngTemplateStage
  
deriving instance Data RngTemplateStage

-- | A stage where the annotation controls how the original source code can be
-- retrieved from the AST. A source template is assigned to each node.
-- It has holes where the content of an other node should be printed.
data SrcTemplateStage

deriving instance Data SrcTemplateStage

data Dom name

data IdDom

deriving instance (Data name, Typeable name) => Data (Dom name)

deriving instance Data IdDom

type SemanticInfo (domain :: *) (node :: * -> * -> *) = SemanticInfo' domain (SemaInfoClassify node)

data SameInfoNameCls
data SameInfoExprCls
data SameInfoImportCls
data SameInfoModuleCls
data SameInfoDefaultCls

type family SemaInfoClassify (node :: * -> * -> *) where
  SemaInfoClassify SimpleName = SameInfoNameCls
  SemaInfoClassify Expr       = SameInfoExprCls
  SemaInfoClassify ImportDecl = SameInfoImportCls
  SemaInfoClassify AST.Module = SameInfoModuleCls
  SemaInfoClassify a          = SameInfoDefaultCls

type family SemanticInfo' (domain :: *) (nodecls :: *)

type instance SemanticInfo' (Dom n) SameInfoNameCls = NameInfo n
type instance SemanticInfo' (Dom n) SameInfoExprCls = ScopeInfo
type instance SemanticInfo' (Dom n) SameInfoImportCls = ImportInfo n
type instance SemanticInfo' (Dom n) SameInfoModuleCls = ModuleInfo GHC.Name
type instance SemanticInfo' (Dom n) SameInfoDefaultCls = NoSemanticInfo

type instance SemanticInfo' IdDom SameInfoNameCls = CNameInfo
type instance SemanticInfo' IdDom SameInfoExprCls = ScopeInfo
type instance SemanticInfo' IdDom SameInfoImportCls = ImportInfo GHC.Id
type instance SemanticInfo' IdDom SameInfoModuleCls = ModuleInfo GHC.Id
type instance SemanticInfo' IdDom SameInfoDefaultCls = NoSemanticInfo

-- | Class for domain configuration markers
class ( Typeable d
      , Data d
      , SemanticInfo' d SameInfoDefaultCls ~ NoSemanticInfo
      , Data (SemanticInfo' d SameInfoNameCls)
      , Data (SemanticInfo' d SameInfoExprCls)
      , Data (SemanticInfo' d SameInfoImportCls)
      , Data (SemanticInfo' d SameInfoModuleCls)
      , Show (SemanticInfo' d SameInfoNameCls)
      , Show (SemanticInfo' d SameInfoExprCls)
      , Show (SemanticInfo' d SameInfoImportCls)
      , Show (SemanticInfo' d SameInfoModuleCls)
      ) => Domain d where

instance ( Typeable d
         , Data d
         , SemanticInfo' d SameInfoDefaultCls ~ NoSemanticInfo
         , Data (SemanticInfo' d SameInfoNameCls)
         , Data (SemanticInfo' d SameInfoExprCls)
         , Data (SemanticInfo' d SameInfoImportCls)
         , Data (SemanticInfo' d SameInfoModuleCls)
         , Show (SemanticInfo' d SameInfoNameCls)
         , Show (SemanticInfo' d SameInfoExprCls)
         , Show (SemanticInfo' d SameInfoImportCls)
         , Show (SemanticInfo' d SameInfoModuleCls)
         ) => Domain d where


class ( Data (SemanticInfo' d (SemaInfoClassify e))
      , Show (SemanticInfo' d (SemaInfoClassify e))
      , Domain d
      ) => DomainWith e d where

instance ( Data (SemanticInfo' d (SemaInfoClassify e))
         , Show (SemanticInfo' d (SemaInfoClassify e))
         , Domain d
         ) => DomainWith e d where

-- | Extracts the concrete range corresponding to a given span.
-- In case of lists and optional elements, it may not contain the elements inside.
class HasRange a where
  getRange :: a -> SrcSpan

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
    deriving (Show, Data)
  data ListInfo RangeStage = ListPos  { _listBefore :: String
                                      , _listAfter :: String
                                      , _listDefaultSep :: String
                                      , _listIndented :: Bool
                                      , _listPos :: SrcLoc 
                                      }
    deriving (Show, Data)
  data OptionalInfo RangeStage = OptionalPos { _optionalBefore :: String
                                             , _optionalAfter :: String 
                                             , _optionalPos :: SrcLoc 
                                             }
    deriving (Show, Data)


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

-- | A non-existing AST part
annNothing :: NodeInfo (SemanticInfo d (AnnMaybe e)) (OptionalInfo s) -> AnnMaybe e d s
annNothing a = AnnMaybe a Nothing


-- * Info types

instance HasRange (SpanInfo RangeStage) where
  getRange (NodeSpan sp) = sp

instance HasRange (ListInfo RangeStage) where
  getRange ListPos{_listPos = pos} = srcLocSpan pos

instance HasRange (OptionalInfo RangeStage) where
  getRange OptionalPos{_optionalPos = pos} = srcLocSpan pos

instance SourceInfo stage => HasRange (Ann elem dom stage) where
  getRange (Ann a _) = getRange (a ^. sourceInfo)

instance SourceInfo stage => HasRange (AnnList elem dom stage) where
  getRange (AnnList a _) = getRange (a ^. sourceInfo)

instance SourceInfo stage => HasRange (AnnMaybe elem dom stage) where
  getRange (AnnMaybe a _) = getRange (a ^. sourceInfo)

class ApplySemaChange cls where 
  appSemaChange :: SemaTrf f dom1 dom2 -> SemanticInfo' dom1 cls -> f (SemanticInfo' dom2 cls)

instance ApplySemaChange SameInfoNameCls where appSemaChange = trfSemaNameCls
instance ApplySemaChange SameInfoExprCls where appSemaChange = trfSemaExprCls
instance ApplySemaChange SameInfoImportCls where appSemaChange = trfSemaImportCls
instance ApplySemaChange SameInfoModuleCls where appSemaChange = trfSemaModuleCls
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
  sourceInfoTraverseUp trf desc asc (Ann (NodeInfo sema src) e) 
    = Ann <$> (NodeInfo sema <$> trfSpanInfo trf src) <*> (desc *> sourceInfoTraverseUp trf desc asc e <* asc)
  sourceInfoTraverseDown trf desc asc (Ann (NodeInfo sema src) e) 
    = flip Ann <$> (desc *> sourceInfoTraverseDown trf desc asc e <* asc) <*> (NodeInfo sema <$> trfSpanInfo trf src)

instance SourceInfoTraversal e => SourceInfoTraversal (AnnList e) where
  sourceInfoTraverse trf (AnnList (NodeInfo sema src) e) 
    = AnnList <$> (NodeInfo sema <$> trfListInfo trf src) <*> mapM (sourceInfoTraverse trf) e
  sourceInfoTraverseUp trf desc asc (AnnList (NodeInfo sema src) e) 
    = AnnList <$> (NodeInfo sema <$> trfListInfo trf src) <*> (desc *> mapM (sourceInfoTraverseUp trf desc asc) e <* asc)
  sourceInfoTraverseDown trf desc asc (AnnList (NodeInfo sema src) e) 
    = flip AnnList <$> (desc *> mapM (sourceInfoTraverseDown trf desc asc) e <* asc) <*> (NodeInfo sema <$> trfListInfo trf src)

instance SourceInfoTraversal e => SourceInfoTraversal (AnnMaybe e) where
  sourceInfoTraverse trf (AnnMaybe (NodeInfo sema src) e) 
    = AnnMaybe <$> (NodeInfo sema <$> trfOptionalInfo trf src) <*> sequence (fmap (sourceInfoTraverse trf) e)
  sourceInfoTraverseUp trf desc asc (AnnMaybe (NodeInfo sema src) e) 
    = AnnMaybe <$> (NodeInfo sema <$> trfOptionalInfo trf src) <*> (desc *> sequence (fmap (sourceInfoTraverseUp trf desc asc) e) <* asc)
  sourceInfoTraverseDown trf desc asc (AnnMaybe (NodeInfo sema src) e) 
    = flip AnnMaybe <$> (desc *> sequence (fmap (sourceInfoTraverseDown trf desc asc) e) <* asc) <*> (NodeInfo sema <$> trfOptionalInfo trf src)

