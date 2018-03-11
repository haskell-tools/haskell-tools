module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleContextsChecker where

import Control.Reference ((^.))

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad


chkFlexibleContexts :: CheckNode Context
chkFlexibleContexts = conditional chkFlexibleContexts' FlexibleContexts

chkFlexibleContexts' :: CheckNode Context
chkFlexibleContexts' ctx@(Context a) = chkAssertion a >> return ctx

chkAssertion :: CheckNode Assertion
chkAssertion a@(ClassAssert _ annTys)
  | tys <- annTys ^. annListElems
  , all hasTyVarHead tys
  = return a
  | otherwise = addOccurence FlexibleContexts a
chkAssertion a@(InfixAssert lhs _ rhs)
  | hasTyVarHead lhs && hasTyVarHead rhs = return a
  | otherwise = addOccurence FlexibleContexts a
chkAssertion a@(TupleAssert xs) = do
  mapM_ chkAssertion xs
  return a
chkAssertion a@ImplicitAssert{} = return a
