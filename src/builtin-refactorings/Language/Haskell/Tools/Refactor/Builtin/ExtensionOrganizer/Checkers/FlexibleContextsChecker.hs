module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.FlexibleContextsChecker where

import Kind (returnsConstraintKind)
import PrelNames (eqTyConName)
import qualified Name as GHC

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
chkAssertion a@(InfixAssert lhs op rhs)
  -- If the constraint is of form a ~ b, then we dont need to check
  | Just name <- semanticsName op
  , name == eqTyConName
  = return a
  -- If the constraint has only type variable head, it doesn't need FlexibleContexts
  | hasTyVarHead lhs && hasTyVarHead rhs = return a
  -- In any other case, we keep FlexibleContexts
  | otherwise = addOccurence FlexibleContexts a
chkAssertion a@(TupleAssert xs) = do
  mapM_ chkAssertion xs
  return a
chkAssertion a@ImplicitAssert{} = return a

chkFlexibleContextsDecl :: CheckNode Decl
chkFlexibleContextsDecl = conditional chkFlexibleContextsDecl' FlexibleContexts

chkFlexibleContextsDecl' :: CheckNode Decl
chkFlexibleContextsDecl' d@(TypeDecl dh rhs) = do
  let ty = typeOrKindFromId . declHeadQName $ dh
  when (hasConstraintKind ty || returnsConstraintKind ty)
       (chkClassesInside rhs)
  return d
chkFlexibleContextsDecl' d = return d

-- | Checks whether all type class applications in a type synonym rhs
-- (that has kind Constraint) have only type variable heads.
-- Returns False if a lookup inside is not succesful.
-- If it isn't applied to a type that has kind Constraint, it may give
-- false positive results.
chkClassesInside :: Type -> ExtMonad ()
chkClassesInside (TupleType annTys) =
  mapM_ chkClassesInside (annTys ^. annListElems)
chkClassesInside t = maybe (addFC t) f (splitTypeAppMaybe t)
  where
    addFC :: Type -> ExtMonad ()
    addFC = addOccurence_ FlexibleContexts

    f :: (GHC.Name, [Type]) -> ExtMonad ()
    f (n,args) = do
       isClass <- isJustT . lookupClass $ n
       when (n /= eqTyConName && isClass && any (not . hasTyVarHead) args)
            (addFC t)

    -- Separates the type into its head and its arguments.
    -- This functions should only be applied to types that have kind Constraint.
    splitTypeAppMaybe :: Type -> Maybe (GHC.Name, [Type])
    splitTypeAppMaybe (TypeApp f arg) = do
      (f', args) <- splitTypeAppMaybe f
      return (f', args ++ [arg])
    splitTypeAppMaybe (InfixTypeApp lhs op rhs) = do
      opName <- semanticsName op
      return (opName, [lhs,rhs])
    splitTypeAppMaybe (VarType n) = do
      sname <- semanticsName n
      return (sname, [])
    splitTypeAppMaybe (ParenType t) = splitTypeAppMaybe t
    splitTypeAppMaybe (KindedType t _) = splitTypeAppMaybe t
    splitTypeAppMaybe _ = Nothing
