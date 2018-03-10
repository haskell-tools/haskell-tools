{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Language.Haskell.Tools.Refactor.Utils.NameLookup where

import qualified GHC

import Data.Maybe (maybeToList)

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite
import Language.Haskell.Tools.Refactor.Utils.Maybe


instance HasNameInfo' Operator where
  semanticsName = opSemName

instance HasNameInfo' DeclHead where
  semanticsName = declHeadSemName

instance HasNameInfo' InstanceHead where
  semanticsName = instHeadSemName

opSemName :: Operator -> Maybe GHC.Name
opSemName = semanticsName . (^. operatorName)

declHeadSemName :: DeclHead -> Maybe GHC.Name
declHeadSemName (NameDeclHead n)       = semanticsName n
declHeadSemName (ParenDeclHead dh)     = declHeadSemName dh
declHeadSemName (DeclHeadApp dh _)     = declHeadSemName dh
declHeadSemName (InfixDeclHead _ op _) = opSemName op

instHeadSemName :: InstanceHead -> Maybe GHC.Name
instHeadSemName (InstanceHead n)         = semanticsName n
instHeadSemName (InfixInstanceHead _ op) = opSemName op
instHeadSemName (ParenInstanceHead ih)   = instHeadSemName ih
instHeadSemName (AppInstanceHead ih _)   = instHeadSemName ih

-- | Collects the qualified names of the class heads in an assertion.
assertionQNames :: Assertion -> [QualifiedName]
assertionQNames (ClassAssert n _)    = [n ^. simpleName]
assertionQNames (InfixAssert _ op _) = [op ^. operatorName]
assertionQNames (ImplicitAssert n _) = [n ^. simpleName]
assertionQNames (TupleAssert xs)     = concatMap assertionQNames xs
assertionQNames _                    = []

-- | Collects the semantic names of the class heads in an assertion.
assertionSemNames :: Assertion -> [GHC.Name]
assertionSemNames (ClassAssert n _)    = maybeToList . semanticsName $ n
assertionSemNames (InfixAssert _ op _) = maybeToList . opSemName $ op
assertionSemNames (ImplicitAssert n _) = maybeToList . semanticsName $ n
assertionSemNames (TupleAssert xs)     = concatMap assertionSemNames xs
assertionSemNames _ = []
