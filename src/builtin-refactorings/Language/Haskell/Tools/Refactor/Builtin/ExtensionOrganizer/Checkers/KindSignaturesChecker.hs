module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.KindSignaturesChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkKindSignaturesKind :: CheckNode Kind
chkKindSignaturesKind = conditional chkKind KindSignatures
  where chkKind = addEvidence KindSignatures
