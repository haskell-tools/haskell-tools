{-# LANGUAGE TypeFamilies
           , DataKinds
           #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.GHCTypeTraversal.AppSelector where

import Var
import TyCon
import TyCoRep
import CoAxiom

import Data.Generics.ClassyPlate
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Instances.AppSelector()

type family HasChecker ty where
  HasChecker Type             = 'True
  HasChecker Var              = 'True
  HasChecker TyCon            = 'True
  HasChecker Coercion         = 'True
  HasChecker UnivCoProvenance = 'True
  HasChecker _                = 'False

--type instance AppSelector Checkable node = HasChecker node
