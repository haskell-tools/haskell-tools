module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.DefaultSignaturesChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkDefaultSigs :: CheckNode ClassElement
chkDefaultSigs ce@ClsDefaultSig{} = addEvidence DefaultSignatures ce
chkDefaultSigs x = return x
