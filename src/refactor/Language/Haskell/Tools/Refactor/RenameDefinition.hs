{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Tools.Refactor.RenameDefinition (renameDefinition) where

import Name hiding (Name)
import GHC (Ghc)
import qualified GHC

import Control.Reference hiding (element)
import Data.Data
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen

type STWithNames n = NodeInfo (SemanticInfo n) SourceTemplate

renameDefinition :: forall n . (NamedThing n, Data n) => GHC.Name -> String -> Ann Module (STWithNames n) -> Ghc (Ann Module (STWithNames n))
renameDefinition toChange newName mod
  = if containsDefinition toChange mod 
       then return $ biplateRef & filtered isTheName .- changeName newName $ mod
       else error "The definition to rename was not found"
  where isTheName :: Ann Name (STWithNames n) -> Bool
        isTheName n = fmap getName (n ^? semantics&nameInfo) == Just toChange

-- | We have to perform this check, otherwise the transformation could introduce semantic errors
containsDefinition :: forall n . NamedThing n => GHC.Name -> Ann Module (STWithNames n) -> Bool
containsDefinition name mod 
  = any ((== Just name) . fmap getName . getTopLevelDeclName) (mod ^? element & modDecl & annList & element :: [Decl (STWithNames n)])

changeName :: (NamedThing n, Data n) => String -> Ann Name (STWithNames n) -> Ann Name (STWithNames n)
changeName str = element & unqualifiedName .= mkSimpleName str
       