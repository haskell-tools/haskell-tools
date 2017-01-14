-- | Generation of binding-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkMatch@ creates the annotated version of the @UMatch@ constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Binds where

import Data.String (IsString(..))
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Patterns (mkVarPat)
import Language.Haskell.Tools.AST.Gen.Utils (mkAnn, mkAnnList, mkAnnMaybe)
import Language.Haskell.Tools.Transform

-- | A simplified function to generate simple value bindings without local definitions, guards or complex lhs.
mkSimpleBind' :: Name dom -> Expr dom -> ValueBind dom
mkSimpleBind' n e = mkSimpleBind (mkVarPat n) (mkUnguardedRhs e) Nothing

-- | Creates a value binding (@ v = "12" @).
mkSimpleBind :: Pattern dom -> Rhs dom -> Maybe (LocalBinds dom) -> ValueBind dom
mkSimpleBind p r l = mkAnn (child <> child <> child) (USimpleBind p r (mkAnnMaybe opt l))

-- | Creates a function binding (@ f 0 = 1; f x = x @). All matches must have the same name.
mkFunctionBind :: [Match dom] -> ValueBind dom
mkFunctionBind = mkAnn child . UFunBind . mkAnnList (indented list)

-- | A simplified function for creating function bindings without local definitions or guards.
mkFunctionBind' :: Name dom -> [([Pattern dom], Expr dom)] -> ValueBind dom
mkFunctionBind' name matches = mkFunctionBind $ map (\(args, rhs) -> mkMatch (mkMatchLhs name args) (mkUnguardedRhs rhs) Nothing) matches

-- | Creates a clause of function binding   
mkMatch :: MatchLhs dom -> Rhs dom -> Maybe (LocalBinds dom) -> Match dom
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ UMatch lhs rhs (mkAnnMaybe (after " " opt) locs)

-- | Creates a match lhs with the function name and parameter names (@ f a b @)
mkMatchLhs :: Name dom -> [Pattern dom] -> MatchLhs dom
mkMatchLhs n pats = mkAnn (child <> child) $ UNormalLhs n (mkAnnList (after " " $ separatedBy " " list) pats)

-- | Creates an infix match lhs for an operator (@ a + b @)
mkInfixLhs :: Pattern dom -> Operator dom -> Pattern dom -> [Pattern dom] -> MatchLhs dom
mkInfixLhs lhs op rhs pats 
  = mkAnn (child <> child <> child <> child) $ UInfixLhs lhs op rhs (mkAnnList (after " " $ separatedBy " " list) pats)

-- | Local bindings attached to a declaration (@ where x = 42 @)
mkLocalBinds :: [LocalBind dom] -> MaybeLocalBinds dom
-- TODO: make the indentation automatic
mkLocalBinds = mkAnnMaybe (relativeIndented 2 $ after "\nwhere " opt)
                     . Just . mkAnn child . ULocalBinds . mkAnnList (indented list)

mkLocalBinds' :: [LocalBind dom] -> LocalBinds dom
mkLocalBinds' = mkAnn (" where " <> child) . ULocalBinds . mkAnnList (indented list)

-- | Creates a local binding for a value
mkLocalValBind :: ValueBind dom -> LocalBind dom
mkLocalValBind = mkAnn child . ULocalValBind

-- | Creates a local type signature
mkLocalTypeSig :: TypeSignature dom -> LocalBind dom
mkLocalTypeSig = mkAnn child . ULocalSignature

-- | Creates a local fixity declaration
mkLocalFixity :: FixitySignature dom -> LocalBind dom
mkLocalFixity = mkAnn child . ULocalFixity

-- | Creates a type signature (@ f :: Int -> Int @)
mkTypeSignature :: Name dom -> Type dom -> TypeSignature dom
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (UTypeSignature (mkAnnList (separatedBy ", " list) [n]) t)

-- | Creates a left-associative fixity declaration (@ infixl 5 +, - @).
mkInfixL :: Int -> Operator dom -> FixitySignature dom
mkInfixL prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixl" AssocLeft) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (separatedBy ", " list) [op])

-- | Creates a right-associative fixity declaration (@ infixr 5 +, - @).
mkInfixR :: Int -> Operator dom -> FixitySignature dom
mkInfixR prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixr" AssocRight) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (separatedBy ", " list) [op])

-- | Creates a non-associative fixity declaration (@ infix 5 +, - @).
mkInfix :: Int -> Operator dom -> FixitySignature dom
mkInfix prec op = mkAnn (child <> " " <> child <> " " <> child) 
                    $ UFixitySignature (mkAnn "infix" AssocNone) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (separatedBy ", " list) [op])

-- | Creates an unguarded right-hand-side (@ = 3 @)
mkUnguardedRhs :: Expr dom -> Rhs dom
mkUnguardedRhs = mkAnn (" = " <> child) . UUnguardedRhs

-- | Creates an unguarded right-hand-side (@ | x == 1 = 3; | otherwise = 4 @)
mkGuardedRhss :: [GuardedRhs dom] -> Rhs dom
mkGuardedRhss = mkAnn child . UGuardedRhss . mkAnnList (indented list)

-- | Creates a guarded right-hand side of a value binding (@ | x > 3 = 2 @)    
mkGuardedRhs :: [RhsGuard dom] -> Expr dom -> GuardedRhs dom
mkGuardedRhs guards expr = mkAnn ("| " <> child <> " = " <> child) $ UGuardedRhs (mkAnnList (separatedBy ", " list) guards) expr

-- | Creates a bind statement in a pattern guard (@ Just v <- x @)
mkGuardBind :: Pattern dom -> Expr dom -> RhsGuard dom
mkGuardBind pat expr = mkAnn (child <> " <- " <> child) $ UGuardBind pat expr

-- | Creates a let statement in a pattern guard (@ let x = 3 @)
mkGuardLet :: [LocalBind dom] -> RhsGuard dom
mkGuardLet = mkAnn ("let " <> child) . UGuardLet . mkAnnList (indented list)

-- | Creates an expression to check for a pattern guard
mkGuardCheck :: Expr dom -> RhsGuard dom
mkGuardCheck = mkAnn child . UGuardCheck

-- pragmas are omitted
