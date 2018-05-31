module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.UnboxedTuplesChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkUnboxedTuplesExpr :: CheckNode Expr
chkUnboxedTuplesExpr = conditional chkUnboxedTuplesExpr' UnboxedTuples

chkUnboxedTuplesPat :: CheckNode Pattern
chkUnboxedTuplesPat = conditional chkUnboxedTuplesPat' UnboxedTuples

chkUnboxedTuplesType :: CheckNode Type
chkUnboxedTuplesType = conditional chkUnboxedTuplesType' UnboxedTuples


chkUnboxedTuplesExpr' :: CheckNode Expr
chkUnboxedTuplesExpr' e@(UnboxedTuple        _) = addEvidence UnboxedTuples e
chkUnboxedTuplesExpr' e@(UnboxedTupleSection _) = addEvidence UnboxedTuples e
chkUnboxedTuplesExpr' e = return e

chkUnboxedTuplesPat' :: CheckNode Pattern
chkUnboxedTuplesPat' p@(UnboxTuplePat _) = addEvidence UnboxedTuples p
chkUnboxedTuplesPat' p = return p

chkUnboxedTuplesType' :: CheckNode Type
chkUnboxedTuplesType' t@(UnboxedTupleType _) = addEvidence UnboxedTuples t
chkUnboxedTuplesType' t = return t
