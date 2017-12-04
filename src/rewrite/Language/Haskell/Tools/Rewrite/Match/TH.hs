-- | Pattern matching on Template Haskell AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.Rewrite.Match.TH where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | A simple name splice: @$generateX@
pattern IdSplice :: Name -> Splice
pattern IdSplice n <- Ann _ (UIdSplice n)

-- | A splice with parentheses: @$(generate input)@
pattern ParenSplice :: Expr -> Splice
pattern ParenSplice e <- Ann _ (UParenSplice e)

-- | Template haskell quasi-quotation: @[quoter|str]@  
pattern QuasiQuote :: Name -> String -> QuasiQuote
pattern QuasiQuote n qqStr <- Ann _ (UQuasiQuote n (Ann _ (QQString qqStr)))

-- | Expression bracket (@ [| x + y |] @)
pattern ExprBracket :: Expr -> Bracket
pattern ExprBracket e <- Ann _ (UExprBracket e)

-- | Pattern bracket (@ [p| Point x y |] @)
pattern PatternBracket :: Pattern -> Bracket
pattern PatternBracket p <- Ann _ (UPatternBracket p)

-- | Type bracket (@ [t| (Int,Int) |] @)
pattern TypeBracket :: Type -> Bracket
pattern TypeBracket t <- Ann _ (UTypeBracket t)

-- | Declaration bracket (@ [d| f :: Int -> Int; f x = x*x |] @)
pattern DeclsBracket :: DeclList -> Bracket
pattern DeclsBracket d <- Ann _ (UDeclsBracket d)




