-- | Pattern matching on Template Haskell AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.Rewrite.Match.TH where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | A simple name splice: @$generateX@
pattern IdSplice :: Name dom -> Splice dom
pattern IdSplice n <- Ann _ (UIdSplice n)

-- | A splice with parentheses: @$(generate input)@
pattern ParenSplice :: Expr dom -> Splice dom
pattern ParenSplice e <- Ann _ (UParenSplice e)

-- | Template haskell quasi-quotation: @[quoter|str]@  
pattern QuasiQuote :: Name dom -> String -> QuasiQuote dom
pattern QuasiQuote n qqStr <- Ann _ (UQuasiQuote n (Ann _ (QQString qqStr)))

-- | Expression bracket (@ [| x + y |] @)
pattern ExprBracket :: Expr dom -> Bracket dom
pattern ExprBracket e <- Ann _ (UExprBracket e)

-- | Pattern bracket (@ [p| Point x y |] @)
pattern PatternBracket :: Pattern dom -> Bracket dom
pattern PatternBracket p <- Ann _ (UPatternBracket p)

-- | Type bracket (@ [t| (Int,Int) |] @)
pattern TypeBracket :: Type dom -> Bracket dom
pattern TypeBracket t <- Ann _ (UTypeBracket t)

-- | Declaration bracket (@ [d| f :: Int -> Int; f x = x*x |] @)
pattern DeclsBracket :: DeclList dom -> Bracket dom
pattern DeclsBracket d <- Ann _ (UDeclsBracket d)




