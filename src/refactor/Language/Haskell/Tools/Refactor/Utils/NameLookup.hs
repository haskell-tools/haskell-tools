{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.Tools.Refactor.Utils.NameLookup where

import qualified GHC

import Data.Maybe (maybeToList)

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite
import Language.Haskell.Tools.Refactor.Utils.Maybe()


instance HasNameInfo' GHC.Name where
  semanticsName = Just <$> id

instance HasNameInfo' Operator where
  semanticsName = opSemName

instance HasNameInfo' DeclHead where
  semanticsName = declHeadSemName

instance HasNameInfo' InstanceHead where
  semanticsName = instHeadSemName


opSemName :: Operator -> Maybe GHC.Name
opSemName = semanticsName . (^. operatorName)

declHeadQName :: DeclHead -> QualifiedName
declHeadQName (NameDeclHead n)       = n ^. simpleName
declHeadQName (ParenDeclHead dh)     = declHeadQName dh
declHeadQName (DeclHeadApp dh _)     = declHeadQName dh
declHeadQName (InfixDeclHead _ op _) = op ^. operatorName

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

-- | Extracts the name of a type.
-- In case of a type application, it finds the type being applied.
-- It works only for unambiguous types, so it won't work for tuples.
nameFromType :: Type -> Maybe Name
nameFromType (TypeApp f _)    = nameFromType f
nameFromType (ParenType x)    = nameFromType x
nameFromType (ListType t)     = nameFromType t
nameFromType (KindedType t _) = nameFromType t
nameFromType (BangType t)     = nameFromType t
nameFromType (LazyType t)     = nameFromType t
nameFromType (UnpackType t)   = nameFromType t
nameFromType (NoUnpackType t) = nameFromType t
nameFromType (VarType x)      = Just x
nameFromType _                = Nothing
