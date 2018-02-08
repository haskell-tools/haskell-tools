module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.NameLookup where

import qualified GHC

import Control.Reference ((^.))

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

import Control.Monad.Trans.Maybe (MaybeT(..))

opSemName :: Operator -> MaybeT ExtMonad GHC.Name
opSemName = liftMaybe . semanticsName . (^. operatorName)

declHeadSemName :: DeclHead -> MaybeT ExtMonad GHC.Name
declHeadSemName (NameDeclHead n)       = liftMaybe . semanticsName $ n
declHeadSemName (ParenDeclHead dh)     = declHeadSemName dh
declHeadSemName (DeclHeadApp dh _)     = declHeadSemName dh
declHeadSemName (InfixDeclHead _ op _) = opSemName op
