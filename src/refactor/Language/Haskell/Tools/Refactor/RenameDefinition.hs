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

renameDefinition' :: forall dom . DomainRenameDefinition dom => RealSrcSpan -> String -> Ann Module dom SrcTemplateStage -> RefactoredModule dom
renameDefinition' sp str mod
  = case (getNodeContaining sp mod :: Maybe (Ann SimpleName dom SrcTemplateStage)) >>= (fmap getName . (semanticsName =<<) . (^? semantics)) of 
      Just name -> do let sameNames = bindsWithSameName name (mod ^? biplateRef) 
                      foldM (\ast n -> renameDefinition n str ast) mod (name : sameNames) 
        where bindsWithSameName :: GHC.Name -> [Ann FieldWildcard dom SrcTemplateStage] -> [GHC.Name]
              bindsWithSameName name wcs = catMaybes $ map ((lookup name) . (^. semantics&implicitFieldBindings)) wcs
      Nothing -> refactError "No name is selected"

renameDefinition :: DomainRenameDefinition dom => GHC.Name -> String -> Ann Module dom SrcTemplateStage -> RefactoredModule dom
renameDefinition toChange newName mod 
    = do nameCls <- classifyName toChange
         (res,defFound) <- runStateT (biplateRef !~ changeName toChange newName $ mod) False
         if | not (nameValid nameCls newName) -> refactError "The new name is not valid"
            | not defFound -> refactError "The definition to rename was not found"
            | otherwise -> return res
  where     
    changeName :: DomainRenameDefinition dom => GHC.Name -> String -> Ann SimpleName dom SrcTemplateStage -> StateT Bool (Refactor dom) (Ann SimpleName dom SrcTemplateStage)
    changeName toChange str elem 
      = if | fmap getName (semanticsName (elem ^. semantics)) == Just toChange
               && semanticsDefining (elem ^. semantics) == False
               && any @[] ((str ==) . occNameString . getOccName) (semanticsScope (elem ^. semantics) ^? Ref.element 0 & traversal)
             -> lift $ refactError "The definition clashes with an existing one" -- name clash with an external definition
           | fmap getName (semanticsName (elem ^. semantics)) == Just toChange
             -> do modify (|| semanticsDefining (elem ^. semantics))
                   return $ element & unqualifiedName .= mkNamePart str $ elem
           | let namesInScope = semanticsScope (elem ^. semantics)
              in case semanticsName (elem ^. semantics) of 
                   Just (getName -> exprName) -> str == occNameString (getOccName exprName) && sameNamespace toChange exprName
                                                   && conflicts toChange exprName namesInScope
                   Nothing -> False -- ambiguous names
             -> lift $ refactError "The definition clashes with an existing one" -- local name clash
           | otherwise -> return elem

conflicts :: GHC.Name -> GHC.Name -> [[GHC.Name]] -> Bool
conflicts overwrites overwritten (scopeBlock : scope) 
  | overwritten `elem` scopeBlock && overwrites `notElem` scopeBlock = False
  | overwrites `elem` scopeBlock = True
  | otherwise = conflicts overwrites overwritten scope
conflicts _ _ [] = False

sameNamespace :: GHC.Name -> GHC.Name -> Bool
sameNamespace n1 n2 = occNameSpace (getOccName n1) == occNameSpace (getOccName n2)
