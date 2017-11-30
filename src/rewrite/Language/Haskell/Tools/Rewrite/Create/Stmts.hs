-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.Haskell.Tools.Rewrite.Create.Stmts where

import Language.Haskell.Tools.AST (UCompStmt(..), UListCompBody(..), UStmt'(..))
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList, mkAnnMaybe)
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | Creates a binding statement (@ x <- action @)
mkBindStmt :: Pattern -> Expr -> Stmt
mkBindStmt bound expr = mkAnn (child <> " <- " <> child) $ UBindStmt bound expr

-- | Creates a non-binding statement (@ action @)
mkExprStmt :: Expr -> Stmt
mkExprStmt = mkAnn child . UExprStmt

-- | Creates a let statement (@ let x = 3; y = 4 @)
mkLetStmt :: [LocalBind] -> Stmt
mkLetStmt = mkAnn ("let " <> child) . ULetStmt . mkAnnList (indented list)

-- | Creates a recursive binding statement with (@ rec b <- f a c; c <- f b a @)
mkRecStmt :: [Stmt] -> Stmt
mkRecStmt = mkAnn ("rec " <> child) . URecStmt . mkAnnList (indented list)

-- * List comprehensions

-- | Body of a list comprehension: (@ | x <- [1..10] @)
mkListCompBody :: [CompStmt] -> ListCompBody
mkListCompBody = mkAnn child . UListCompBody . mkAnnList (separatedBy " " list)

-- | Normal monadic statement of a list comprehension
mkCompStmt :: Stmt -> CompStmt
mkCompStmt = mkAnn child . UCompStmt

-- | Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
mkThenStmt :: Expr -> Maybe Expr -> CompStmt
mkThenStmt th by = mkAnn ("then " <> child) 
                     $ UThenStmt th $ mkAnnMaybe (after " by " opt) by

-- | Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
mkGroupStmt :: Maybe Expr -> Maybe Expr -> CompStmt
mkGroupStmt by using = mkAnn ("then group" <> child) 
                         $ UGroupStmt (mkAnnMaybe (after " by " opt) by)
                                      (mkAnnMaybe (after " using " opt) using)

-- * Commands

-- | Creates a binding command (@ x <- action @)
mkBindCmd :: Pattern -> Cmd -> CmdStmt
mkBindCmd bound expr = mkAnn (child <> " <- " <> child) $ UBindStmt bound expr

-- | Creates a non-binding command (@ action @)
mkExprCmd :: Cmd -> CmdStmt
mkExprCmd = mkAnn child . UExprStmt

-- | Creates a let command (@ let x = 3; y = 4 @)
mkLetStmtCmd :: [LocalBind] -> CmdStmt
mkLetStmtCmd = mkAnn ("let " <> child) . ULetStmt . mkAnnList (indented list)

-- | Creates a recursive binding command with (@ rec b <- f a c; c <- f b a @)
mkRecCmd :: [CmdStmt] -> CmdStmt
mkRecCmd = mkAnn ("rec " <> child) . URecStmt . mkAnnList (indented list)

