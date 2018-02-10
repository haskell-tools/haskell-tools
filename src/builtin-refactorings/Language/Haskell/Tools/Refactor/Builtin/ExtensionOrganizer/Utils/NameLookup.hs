module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.NameLookup where

import qualified GHC

import Control.Reference ((^.))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad


opSemName :: Operator -> MaybeT ExtMonad GHC.Name
opSemName = liftMaybe . semanticsName . (^. operatorName)

declHeadSemName :: DeclHead -> MaybeT ExtMonad GHC.Name
declHeadSemName (NameDeclHead n)       = liftMaybe . semanticsName $ n
declHeadSemName (ParenDeclHead dh)     = declHeadSemName dh
declHeadSemName (DeclHeadApp dh _)     = declHeadSemName dh
declHeadSemName (InfixDeclHead _ op _) = opSemName op

instHeadSemName :: InstanceHead -> MaybeT ExtMonad GHC.Name
instHeadSemName (InstanceHead n)         = liftMaybe . semanticsName $ n
instHeadSemName (InfixInstanceHead _ op) = opSemName op
instHeadSemName (ParenInstanceHead ih)   = instHeadSemName ih
instHeadSemName (AppInstanceHead ih _)   = instHeadSemName ih
