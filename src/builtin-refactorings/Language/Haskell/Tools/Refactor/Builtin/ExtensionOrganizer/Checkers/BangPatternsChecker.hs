module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.BangPatternsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkBangPatterns :: CheckNode Pattern
chkBangPatterns = conditional chkBangPatterns' BangPatterns

chkBangPatterns' :: CheckNode Pattern
chkBangPatterns' p@(BangPat _) = addEvidence BangPatterns p
chkBangPatterns' x = return x

{-
  RhsGuard DONE
  MatchLhs DONE
  ValueBind DONE
  Expr DONE
  Alt DONE
  Stmt DONE
  Pattern MORE TESTS
  Bracket ASK
  PatSynRhs DONE
-}
