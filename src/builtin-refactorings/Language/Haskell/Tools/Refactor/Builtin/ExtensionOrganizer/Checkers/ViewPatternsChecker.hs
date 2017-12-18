module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.ViewPatternsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkViewPatterns :: CheckNode Pattern
chkViewPatterns = conditional chkViewPatterns' ViewPatterns

chkViewPatterns' :: CheckNode Pattern
chkViewPatterns' p@(ViewPat _ _) = addOccurence ViewPatterns p
chkViewPatterns' p = return p
