{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Language.Haskell.Tools.AST.SemaInfo (module Language.Haskell.Tools.AST.SemaInfo, getInstances, UsageSpec(..)) where

import Control.Reference
import Data.Maybe
import Data.Data as Data
import Control.Reference
import Control.Monad
import Control.Monad.IO.Class

import BasicTypes as GHC
import DynFlags as GHC
import FamInstEnv as GHC
import qualified GHC
import Id as GHC
import Var
import InstEnv as GHC
import Module as GHC
import Name as GHC
import RdrName as GHC
import SrcLoc as GHC
import Type as GHC
import HscTypes as GHC
import CoAxiom as GHC

import Language.Haskell.Tools.AST.Ann as AST
import Language.Haskell.Tools.AST.Representation.Exprs as AST (UFieldWildcard, UExpr)
import Language.Haskell.Tools.AST.Representation.Modules as AST (UImportDecl, UModule)
import Language.Haskell.Tools.AST.Representation.Names as AST (UQualifiedName)
import Language.Haskell.Tools.AST.Representation.Literals as AST (ULiteral)
import Language.Haskell.Tools.AST.SemaInfoTypes as AST

-- * Literal infos

semaLitType :: Ann ULiteral IdDom st -> GHC.Type
semaLitType lit = lit ^. annotation & semanticInfo & literalType

-- * Name infos

semaName :: Ann UQualifiedName IdDom st -> GHC.Name
semaName = idName . semanticsId

semaId :: Ann UQualifiedName IdDom st -> GHC.Id
semaId = _cnameInfo . (^. annotation&semanticInfo)

semaFixity :: Ann UQualifiedName IdDom st -> Maybe GHC.Fixity
semaFixity = _cnameFixity . (^. annotation&semanticInfo)

semaIsDefining :: Ann UQualifiedName IdDom st -> Bool
semaIsDefining = _semanticsDefining . (^. annotation&semanticInfo)

-- * Expression infos

semaExprScope :: Ann UExpr IdDom st -> Scope
semaExprScope = _nameScopedLocals . (^. annotation&semanticInfo)

semaNameScope :: Ann UExpr IdDom st -> Scope
semaNameScope = _nameScopedLocals . (^. annotation&semanticInfo)

-- * Module infos

semaModule :: Ann UModule IdDom st -> GHC.Module
semaModule = _defModuleName . (^. annotation&semanticInfo)

semaDynFlags :: Ann UModule IdDom st -> GHC.DynFlags
semaDynFlags = _defDynFlags . (^. annotation&semanticInfo)

semaIsBootModule :: Ann UModule IdDom st -> Bool
semaIsBootModule = _defIsBootModule . (^. annotation&semanticInfo)

semaImplicitImports :: Ann UModule IdDom st -> [GHC.Name]
semaImplicitImports = (^? annotation&semanticInfo&implicitNames&traversal&pName)

semaPrelTransMods :: Ann UModule IdDom st -> [Module]
semaPrelTransMods = _prelTransMods . (^. annotation&semanticInfo)

-- * Import infos

semaImportedModule :: Ann UImportDecl IdDom st -> GHC.Module
semaImportedModule = _importedModule . (^. annotation&semanticInfo)

semaAvailable :: Ann UImportDecl IdDom st -> [GHC.Name]
semaAvailable = _availableNames . (^. annotation&semanticInfo)

semaImported :: Ann UImportDecl IdDom st -> [GHC.Name]
semaImported = (^? annotation&semanticInfo&importedNames&traversal&pName)

semaTransMods :: Ann UImportDecl IdDom st -> [Module]
semaTransMods = _importTransMods . (^. annotation&semanticInfo)

-- * Information about implicitly bounded fields

semaImplicitFlds :: Ann UImportDecl IdDom st -> [(GHC.Name, GHC.Name)]
semaImplicitFlds = _implicitFieldBindings . (^. annotation&semanticInfo)

-- * Utility functions

-- | Gets the class and family instances from a module.
getInstances :: [Module] -> GHC.Ghc ([ClsInst], [FamInst])
getInstances mods = do
  env <- GHC.getSession
  eps <- liftIO $ hscEPS env
  let homePkgs = catMaybes $ map (lookupHpt (hsc_HPT env) . GHC.moduleName) mods
      (hptInsts, hptFamInsts) = hptInstances env (`elem` map GHC.moduleName mods)
      isFromMods inst = maybe False (`elem` mods) $ nameModule_maybe $ Var.varName $ is_dfun inst
      famIsFromMods inst = maybe False (`elem` mods) $ nameModule_maybe $ co_ax_name $ fi_axiom inst
      epsInsts = filter isFromMods $ instEnvElts $ eps_inst_env eps
      epsFamInsts = filter famIsFromMods $ famInstEnvElts $ eps_fam_inst_env eps
  return (hptInsts ++ epsInsts, hptFamInsts ++ epsFamInsts)

