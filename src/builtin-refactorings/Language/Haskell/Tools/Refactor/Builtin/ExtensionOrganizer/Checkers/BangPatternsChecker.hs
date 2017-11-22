{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.BangPatternsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any.

chkBangPatterns :: CheckNode Pattern
chkBangPatterns = conditional chkBangPatterns' BangPatterns

chkBangPatterns' :: CheckNode Pattern
chkBangPatterns' p@(BangPat _) = addOccurence BangPatterns p
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
