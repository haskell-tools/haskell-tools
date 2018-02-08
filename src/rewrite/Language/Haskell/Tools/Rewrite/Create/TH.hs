-- | Generation of Template Haskell AST fragments for refactorings.
{-# LANGUAGE MonoLocalBinds, OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.TH where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList)
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | A simple name splice: @$generateX@
mkIdSplice :: Name -> Splice
mkIdSplice n = mkAnn ("$" <> child) $ UIdSplice n

-- | A splice with parentheses: @$(generate input)@
mkParenSplice :: Expr -> Splice
mkParenSplice n = mkAnn ("$(" <> child <> ")") $ UParenSplice n

-- | Template haskell quasi-quotation: @[quoter|str]@  
mkQuasiQuote :: Name -> String -> QuasiQuote
mkQuasiQuote n s = mkAnn ("[" <> child <> "|" <> child <> "|]")
                     $ UQuasiQuote n (mkAnn (fromString s) (QQString s))

-- | Expression bracket (@ [| x + y |] @)
mkExprBracket :: Expr -> Bracket
mkExprBracket = mkAnn ("[|" <> child <> "|]") . UExprBracket

-- | Pattern bracket (@ [p| Point x y |] @)
mkPatternBracket :: Pattern -> Bracket
mkPatternBracket = mkAnn ("[p|" <> child <> "|]") . UPatternBracket

-- | Type bracket (@ [t| (Int,Int) |] @)
mkTypeBracket :: Type -> Bracket
mkTypeBracket = mkAnn ("[t|" <> child <> "|]") . UTypeBracket

-- | Declaration bracket (@ [d| f :: Int -> Int; f x = x*x |] @)
mkDeclsBracket :: [Decl] -> Bracket
mkDeclsBracket = mkAnn ("[d|" <> child <> "|]") . UDeclsBracket . mkAnnList (indented list)

