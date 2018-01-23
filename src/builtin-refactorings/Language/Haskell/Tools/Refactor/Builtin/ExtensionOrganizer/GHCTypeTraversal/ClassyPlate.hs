{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TypeApplications
           , TemplateHaskell
           , DataKinds
           , FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies
           , AllowAmbiguousTypes
           , ScopedTypeVariables
            #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.GHCTypeTraversal.ClassyPlate where

import Data.Generics.ClassyPlate
import Data.Generics.ClassyPlate.TH
import Data.Generics.ClassyPlate.Core

import Var
import TyCon
import TyCoRep
import CoAxiom

--bullshit
import Name (Name)
import Class (Class)
import Unique (Unique)
import UniqDFM (UniqDFM)
import ForeignCall (CType)
import TcType (TcTyVarDetails)
import BasicTypes (TupleSort, LeftOrRight)
import DataCon (DataCon, FieldLbl, EqSpec, HsSrcBang, DataConRep)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad (Checkable)

-- Type
makeClassyPlateConfig OnlyDirect [ Left ('LitTy,0) ] ''Type
-- makeClassyPlateConfig OnlyDirect [ {- Left ('TyVar,0)
--                                  , Left ('TcTyVar,0)
--                                  , Right 'varName
--                                  , Right 'realUnique
--                                  , Right 'idScope
--                                  , Right 'id_details
--                                  , Right 'id_info -}
--                                  ] ''Var
makeClassyPlateConfig OnlyDirect [ {- Right 'tyConUnique
                                 , Right 'tyConName
                                 , Right 'tyConArity
                                 , Right 'tcRepName
                                 , Right 'algTcGadtSyntax
                                 , Right 'algTcFields
                                 , Right 'algTcParent
                                 , Right 'synIsTau
                                 , Right 'synIsFamFree
                                 , Right 'famTcResVar
                                 , Right 'famTcFlav
                                 , Right 'famTcParent
                                 , Right 'famTcInj
                                 , Left ('PrimTyCon, 0)
                                 , Left ('PromotedDataCon, 0)
                                 , Left ('TcTyCon, 0) -}
                                 ] ''TyCon
makeClassyPlateConfig OnlyDirect [] ''Coercion
makeClassyPlateConfig OnlyDirect [ Left ('UnsafeCoerceProv,0)
                                 , Left ('PluginProv,0)
                                 , Left ('HoleProv,0)
                                 ] ''UnivCoProvenance
makeClassyPlateConfig OnlyDirect [ Left ('CoAxiomRule,0)
                                 , Right 'coaxrProves
                                 ] ''CoAxiomRule



makeClassyPlateConfig OnlyDirect [] ''Role
makeClassyPlateConfig OnlyDirect [Right 'nt_etad_rhs] ''AlgTyConRhs
makeClassyPlateConfig OnlyDirect [] ''AlgTyConFlav
makeClassyPlateConfig OnlyDirect [] ''FamTyConFlav
makeClassyPlateConfig OnlyDirect [] ''DataCon
makeClassyPlateConfig OnlyDirect [] ''EqSpec
makeClassyPlateConfig OnlyDirect [] ''TupleSort
makeClassyPlateConfig OnlyDirect [] ''LeftOrRight
makeClassyPlateConfig OnlyDirect [] ''(,)

instance GoodOperationFor Checkable Name => ClassyPlate Checkable Name where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable (TyVarBndr a b) => ClassyPlate Checkable (TyVarBndr a b) where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable (CoAxiom a) => ClassyPlate Checkable (CoAxiom a) where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable (UniqDFM a) => ClassyPlate Checkable (UniqDFM a) where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable Int => ClassyPlate Checkable Int where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable TcTyVarDetails => ClassyPlate Checkable TcTyVarDetails where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable Var => ClassyPlate Checkable Var where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable Unique => ClassyPlate Checkable Unique where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable CType => ClassyPlate Checkable CType where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable Bool => ClassyPlate Checkable Bool where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable (FieldLbl a) => ClassyPlate Checkable (FieldLbl a) where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable HsSrcBang => ClassyPlate Checkable HsSrcBang where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable DataConRep => ClassyPlate Checkable DataConRep where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable Class => ClassyPlate Checkable Class where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable BuiltInSynFamily => ClassyPlate Checkable BuiltInSynFamily where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable RuntimeRepInfo => ClassyPlate Checkable RuntimeRepInfo where
  topDownM_ _ _ = return

instance GoodOperationFor Checkable Injectivity => ClassyPlate Checkable Injectivity where
  topDownM_ _ _ = return
