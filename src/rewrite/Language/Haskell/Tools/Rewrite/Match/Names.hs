-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE PatternSynonyms #-}

module Language.Haskell.Tools.Rewrite.Match.Names where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | A normal operator used as an operator.
pattern NormalOp :: QualifiedName -> Operator
pattern NormalOp n <- Ann _ (UNormalOp n)

-- | A normal name used as an operator with backticks: @ a \`mod\` b @
pattern BacktickOp :: QualifiedName -> Operator
pattern BacktickOp n <- Ann _ (UBacktickOp n)

-- | A normal, non-operator name.
pattern NormalName :: QualifiedName -> Name
pattern NormalName n <- Ann _ (UNormalName n)

-- | Parenthesized name: @ foldl (+) 0 @
pattern ParenName :: QualifiedName -> Name
pattern ParenName n <- Ann _ (UParenName n)

-- | Creates an implicit name: @ ?var @
pattern ImplicitName :: QualifiedName -> Name
pattern ImplicitName n <- Ann _ (UImplicitName n)

-- | Program elements formatted as string literals (import packages, pragma texts)
pattern StringNode :: String -> StringNode
pattern StringNode s <- Ann _ (UStringNode s )

-- | Possibly qualified name.
pattern QualifiedName :: NamePartList -> NamePart -> QualifiedName
pattern QualifiedName quals unqual <- Ann _ (UQualifiedName quals unqual)

-- | Parts of a qualified name.
pattern NamePart :: String -> NamePart
pattern NamePart str <- Ann _ (UNamePart str)
