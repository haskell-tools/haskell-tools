module Language.Haskell.Tools.Refactor.Utils.NameLookup where

import GHC (GhcMonad)
import qualified GHC

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite
import Language.Haskell.Tools.Refactor.Utils.Maybe


opSemName :: GhcMonad m => Operator -> MaybeT m GHC.Name
opSemName = liftMaybe . semanticsName . (^. operatorName)

declHeadSemName :: GhcMonad m => DeclHead -> MaybeT m GHC.Name
declHeadSemName (NameDeclHead n)       = liftMaybe . semanticsName $ n
declHeadSemName (ParenDeclHead dh)     = declHeadSemName dh
declHeadSemName (DeclHeadApp dh _)     = declHeadSemName dh
declHeadSemName (InfixDeclHead _ op _) = opSemName op

instHeadSemName :: GhcMonad m => InstanceHead -> MaybeT m GHC.Name
instHeadSemName (InstanceHead n)         = liftMaybe . semanticsName $ n
instHeadSemName (InfixInstanceHead _ op) = opSemName op
instHeadSemName (ParenInstanceHead ih)   = instHeadSemName ih
instHeadSemName (AppInstanceHead ih _)   = instHeadSemName ih
