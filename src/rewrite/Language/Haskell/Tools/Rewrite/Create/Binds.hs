-- | Generation of binding-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkMatch@ creates the annotated version of the @UMatch@ constructor.
{-# LANGUAGE MonoLocalBinds, OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.Binds where

import Data.String (IsString(..))
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Patterns (mkVarPat)
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList, mkAnnMaybe)
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | A simplified function to generate simple value bindings without local definitions, guards or complex lhs.
mkSimpleBind' :: Name -> Expr -> ValueBind
mkSimpleBind' n e = mkSimpleBind (mkVarPat n) (mkUnguardedRhs e) Nothing

-- | Creates a value binding (@ v = "12" @).
mkSimpleBind :: Pattern -> Rhs -> Maybe LocalBinds -> ValueBind
mkSimpleBind p r l = mkAnn (child <> child <> child) (USimpleBind p r (mkAnnMaybe opt l))

-- | Creates a function binding (@ f 0 = 1; f x = x @). All matches must have the same name.
mkFunctionBind :: [Match] -> ValueBind
mkFunctionBind = mkAnn child . UFunBind . mkAnnList (indented list)

-- | A simplified function for creating function bindings without local definitions or guards.
mkFunctionBind' :: Name -> [([Pattern], Expr)] -> ValueBind
mkFunctionBind' name matches = mkFunctionBind $ map (\(args, rhs) -> mkMatch (mkMatchLhs name args) (mkUnguardedRhs rhs) Nothing) matches

-- | Creates a clause of function binding
mkMatch :: MatchLhs -> Rhs -> Maybe LocalBinds -> Match
mkMatch lhs rhs locs
  = mkAnn (child <> child <> child)
      $ UMatch lhs rhs (mkAnnMaybe (after " " opt) locs)

-- | Creates a match lhs with the function name and parameter names (@ f a b @)
mkMatchLhs :: Name -> [Pattern] -> MatchLhs
mkMatchLhs n pats = mkAnn (child <> child) $ UNormalLhs n (mkAnnList (after " " $ separatedBy " " list) pats)

-- | Creates an infix match lhs for an operator (@ a + b @)
mkInfixLhs :: Pattern -> Operator -> Pattern -> [Pattern] -> MatchLhs
mkInfixLhs lhs op rhs pats
  = mkAnn (child <> child <> child <> child) $ UInfixLhs lhs op rhs (mkAnnList (after " " $ separatedBy " " list) pats)

-- | Local bindings attached to a declaration (@ where x = 42 @)
mkLocalBinds :: [LocalBind] -> MaybeLocalBinds
mkLocalBinds = mkAnnMaybe (relativeIndented 2 $ after "\nwhere " opt)
                     . Just . mkAnn child . ULocalBinds . mkAnnList (indented list)

mkLocalBinds' :: [LocalBind] -> LocalBinds
mkLocalBinds' = mkAnn (" where " <> child) . ULocalBinds . mkAnnList (indented list)

-- | Creates a local binding for a value
mkLocalValBind :: ValueBind -> LocalBind
mkLocalValBind = mkAnn child . ULocalValBind

-- | Creates a local type signature
mkLocalTypeSig :: TypeSignature -> LocalBind
mkLocalTypeSig = mkAnn child . ULocalSignature

-- | Creates a local fixity declaration
mkLocalFixity :: FixitySignature -> LocalBind
mkLocalFixity = mkAnn child . ULocalFixity

-- | Creates a type signature (@ f :: Int -> Int @)
mkTypeSignature :: Name -> Type -> TypeSignature
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (UTypeSignature (mkAnnList (separatedBy ", " list) [n]) t)

-- | Creates a left-associative fixity declaration (@ infixl 5 +, - @).
mkInfixL :: Int -> Operator -> FixitySignature
mkInfixL prec op = mkAnn (child <> " " <> child <> " " <> child)
                     $ UFixitySignature (mkAnn "infixl" AssocLeft) (mkAnnMaybe opt $ Just $ mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (separatedBy ", " list) [op])

-- | Creates a right-associative fixity declaration (@ infixr 5 +, - @).
mkInfixR :: Int -> Operator -> FixitySignature
mkInfixR prec op = mkAnn (child <> " " <> child <> " " <> child)
                     $ UFixitySignature (mkAnn "infixr" AssocRight) (mkAnnMaybe opt $ Just $ mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (separatedBy ", " list) [op])

-- | Creates a non-associative fixity declaration (@ infix 5 +, - @).
mkInfix :: Int -> Operator -> FixitySignature
mkInfix prec op = mkAnn (child <> " " <> child <> " " <> child)
                    $ UFixitySignature (mkAnn "infix" AssocNone) (mkAnnMaybe opt $ Just $ mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (separatedBy ", " list) [op])

-- | Creates an unguarded right-hand-side (@ = 3 @)
mkUnguardedRhs :: Expr -> Rhs
mkUnguardedRhs = mkAnn (" = " <> child) . UUnguardedRhs

-- | Creates an unguarded right-hand-side (@ | x == 1 = 3; | otherwise = 4 @)
mkGuardedRhss :: [GuardedRhs] -> Rhs
mkGuardedRhss = mkAnn child . UGuardedRhss . mkAnnList (indented list)

-- | Creates a guarded right-hand side of a value binding (@ | x > 3 = 2 @)
mkGuardedRhs :: [RhsGuard] -> Expr -> GuardedRhs
mkGuardedRhs guards expr = mkAnn ("| " <> child <> " = " <> child) $ UGuardedRhs (mkAnnList (separatedBy ", " list) guards) expr

-- | Creates a bind statement in a pattern guard (@ Just v <- x @)
mkGuardBind :: Pattern -> Expr -> RhsGuard
mkGuardBind pat expr = mkAnn (child <> " <- " <> child) $ UGuardBind pat expr

-- | Creates a let statement in a pattern guard (@ let x = 3 @)
mkGuardLet :: [LocalBind] -> RhsGuard
mkGuardLet = mkAnn ("let " <> child) . UGuardLet . mkAnnList (indented list)

-- | Creates an expression to check for a pattern guard
mkGuardCheck :: Expr -> RhsGuard
mkGuardCheck = mkAnn child . UGuardCheck

-- pragmas are omitted
