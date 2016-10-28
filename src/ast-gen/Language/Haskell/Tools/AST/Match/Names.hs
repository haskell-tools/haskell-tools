-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Names where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- | A normal operator used as an operator.
pattern NormalOp :: QualifiedName dom -> Operator dom
pattern NormalOp n <- Ann _ (UNormalOp n)

-- | A normal name used as an operator with backticks: @ a `mod` b @
pattern BacktickOp :: QualifiedName dom -> Operator dom
pattern BacktickOp n <- Ann _ (UBacktickOp n)

-- | A normal, non-operator name.
pattern NormalName :: QualifiedName dom -> Name dom
pattern NormalName n <- Ann _ (UNormalName n)

-- | Parenthesized name: @ foldl (+) 0 @
pattern ParenName :: QualifiedName dom -> Name dom
pattern ParenName n <- Ann _ (UParenName n)

-- | Parenthesized name: @ foldl (+) 0 @
pattern ImplicitName :: QualifiedName dom -> Name dom
pattern ImplicitName n <- Ann _ (UImplicitName n)

-- | Program elements formatted as string literals (import packages, pragma texts)
pattern StringNode :: String -> StringNode dom
pattern StringNode s <- Ann _ (UStringNode s )

-- | Possibly qualified name.
pattern QualifiedName :: NamePartList dom -> NamePart dom -> QualifiedName dom
pattern QualifiedName quals unqual <- Ann _ (UQualifiedName quals unqual)

-- | Parts of a qualified name.
pattern NamePart :: String -> NamePart dom
pattern NamePart str <- Ann _ (UNamePart str)
