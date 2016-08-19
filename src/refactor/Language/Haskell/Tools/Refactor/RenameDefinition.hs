{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , MultiWayIf
           , TypeApplications
           , ConstraintKinds
           , TypeFamilies
           , FlexibleContexts
           , ViewPatterns
           #-}
module Language.Haskell.Tools.Refactor.RenameDefinition (renameDefinition, renameDefinition', DomainRenameDefinition) where

import Name hiding (Name)
import GHC (Ghc, TyThing(..), lookupName)
import qualified GHC
import OccName
import SrcLoc
import Outputable

import Control.Reference hiding (element)
import qualified Control.Reference as Ref
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

import Debug.Trace

type DomainRenameDefinition dom = ( Domain dom, HasNameInfo (SemanticInfo' dom SameInfoNameCls), Data (SemanticInfo' dom SameInfoNameCls)
                                  , HasScopeInfo (SemanticInfo' dom SameInfoNameCls), HasDefiningInfo (SemanticInfo' dom SameInfoNameCls)
                                  , SemanticInfo' dom SameInfoWildcardCls ~ ImplicitFieldInfo )

renameDefinition' :: forall dom . DomainRenameDefinition dom => RealSrcSpan -> String -> Refactoring dom
renameDefinition' sp str mod mods
  = case (getNodeContaining sp mod :: Maybe (Ann SimpleName dom SrcTemplateStage)) >>= (fmap getName . (semanticsName =<<) . (^? semantics)) of 
      Just name -> do let sameNames = bindsWithSameName name (mod ^? biplateRef) 
                      renameDefinition name sameNames str mod
        where bindsWithSameName :: GHC.Name -> [Ann FieldWildcard dom SrcTemplateStage] -> [GHC.Name]
              bindsWithSameName name wcs = catMaybes $ map ((lookup name) . (^. semantics&implicitFieldBindings)) wcs
      Nothing -> refactError "No name is selected"

renameDefinition :: DomainRenameDefinition dom => GHC.Name -> [GHC.Name] -> String -> Ann Module dom SrcTemplateStage -> RefactoredModule dom
renameDefinition toChangeOrig toChangeWith newName mod 
    = do nameCls <- classifyName toChangeOrig
         (res,defFound) <- runStateT (biplateRef !~ changeName toChangeOrig toChangeWith newName $ mod) False
         if | not (nameValid nameCls newName) -> refactError "The new name is not valid"
            | not defFound -> refactError "The definition to rename was not found"
            | otherwise -> return res
  where     
    changeName :: DomainRenameDefinition dom => GHC.Name -> [GHC.Name] -> String -> Ann SimpleName dom SrcTemplateStage -> StateT Bool (Refactor dom) (Ann SimpleName dom SrcTemplateStage)
    changeName toChangeOrig toChangeWith str name 
      = if | maybe False (`elem` toChange) actualName
               && semanticsDefining (name ^. semantics) == False
               && any @[] ((str ==) . occNameString . getOccName) (semanticsScope (name ^. semantics) ^? Ref.element 0 & traversal)
             -> lift $ refactError "The definition clashes with an existing one" -- name clash with an external definition
           | maybe False (`elem` toChange) actualName
             -> do when (actualName == Just toChangeOrig) 
                     $ modify (|| semanticsDefining (name ^. semantics))
                   return $ element & unqualifiedName .= mkNamePart str $ name -- found the changed name (or a name that have to be changed too)
           | let namesInScope = semanticsScope (name ^. semantics)
              in case semanticsName (name ^. semantics) of 
                   Just (getName -> exprName) -> str == occNameString (getOccName exprName) && sameNamespace toChangeOrig exprName
                                                   && conflicts toChangeOrig exprName namesInScope
                   Nothing -> False -- ambiguous names
             -> lift $ refactError "The definition clashes with an existing one" -- local name clash
           | otherwise -> return name -- not the changed name, leave as before
      where toChange = toChangeOrig : toChangeWith
            actualName = fmap getName (semanticsName (name ^. semantics))

conflicts :: GHC.Name -> GHC.Name -> [[GHC.Name]] -> Bool
conflicts overwrites overwritten (scopeBlock : scope) 
  | overwritten `elem` scopeBlock && overwrites `notElem` scopeBlock = False
  | overwrites `elem` scopeBlock = True
  | otherwise = conflicts overwrites overwritten scope
conflicts _ _ [] = False

sameNamespace :: GHC.Name -> GHC.Name -> Bool
sameNamespace n1 n2 = occNameSpace (getOccName n1) == occNameSpace (getOccName n2)
