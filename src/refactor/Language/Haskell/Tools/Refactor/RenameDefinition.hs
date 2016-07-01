{-# LANGUAGE ScopedTypeVariables
           , LambdaCase
           , MultiWayIf
           #-}
module Language.Haskell.Tools.Refactor.RenameDefinition (renameDefinition, renameDefinition') where

import Name hiding (Name)
import GHC (Ghc, TyThing(..), lookupName)
import qualified GHC
import OccName
import SrcLoc

import Control.Reference hiding (element)
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

renameDefinition' :: forall n . (NamedThing n, Data n) => RealSrcSpan -> String -> Ann Module (STWithNames n) -> RefactoredModule n
renameDefinition' sp str mod
  = case (getNodeContaining sp mod :: Maybe (Ann SimpleName (STWithNames n))) >>= (fmap getName . (^? semantics&nameInfo)) of 
      Just n -> renameDefinition n str mod
      Nothing -> refactError "No name is selected"

renameDefinition :: forall n . (NamedThing n, Data n) => GHC.Name -> String -> Ann Module (STWithNames n) -> RefactoredModule n
renameDefinition toChange newName mod 
    = do nameCls <- classifyName toChange
         (res,defFound) <- runStateT (biplateRef !~ changeName toChange newName $ mod) False
         if | not (nameValid nameCls newName) -> refactError "The new name is not valid"
            | not defFound -> refactError "The definition to rename was not found"
            | otherwise -> return res
  where     
    changeName :: GHC.Name -> String -> Ann SimpleName (STWithNames n) -> StateT Bool (Refactor n) (Ann SimpleName (STWithNames n))
    changeName toChange str elem 
      = if | fmap getName (elem ^? semantics&nameInfo) == Just toChange
               && (elem ^? semantics&isDefined) == Just False
               && any ((str ==) . occNameString . getOccName) (maybe [] head (elem ^? semantics & scopedLocals))
             -> lift $ refactError "The definition clashes with an existing one" -- name clash with an external definition
           | fmap getName (elem ^? semantics&nameInfo) == Just toChange
             -> do modify (|| fromMaybe False (elem ^? semantics&isDefined)) 
                   return $ element & unqualifiedName .= mkNamePart str $ elem
           | let namesInScope = fromMaybe [] (elem ^? semantics & scopedLocals)
                 actualName = maybe toChange getName (elem ^? semantics & nameInfo)
              in str == occNameString (getOccName actualName) && sameNamespace toChange actualName 
                   && conflicts toChange actualName namesInScope
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
