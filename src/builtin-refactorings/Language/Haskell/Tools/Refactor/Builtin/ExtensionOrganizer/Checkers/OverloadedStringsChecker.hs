module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.OverloadedStringsChecker where

import Type as GHC (eqType)
import TysWiredIn as GHC (stringTy)

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkOverloadedStringsLiteral :: CheckNode Literal
chkOverloadedStringsLiteral = conditional chkLit OverloadedStrings
  where chkLit :: CheckNode Literal
        chkLit lit
          | UStringLit _ <- lit ^. element
          , not . GHC.eqType GHC.stringTy $ semanticsLiteralType lit
          = addEvidence OverloadedStrings lit
          | otherwise = return lit
