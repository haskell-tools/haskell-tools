-- | Generation of Template Haskell AST fragments for refactorings.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.TH where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

-- | A simple name splice: @$generateX@
mkIdSplice :: Name dom -> Splice dom
mkIdSplice n = mkAnn ("$" <> child) $ UIdSplice n

-- | A splice with parentheses: @$(generate input)@
mkParenSplice :: Expr dom -> Splice dom
mkParenSplice n = mkAnn ("$(" <> child <> ")") $ UParenSplice n

-- | Template haskell quasi-quotation: @[quoter|str]@  
mkQuasiQuote :: Name dom -> String -> QuasiQuote dom
mkQuasiQuote n s = mkAnn ("[" <> child <> "|" <> child <> "|]")
                     $ UQuasiQuote n (mkAnn (fromString s) (QQString s))

-- | Expression bracket (@ [| x + y |] @)
mkExprBracket :: Expr dom -> Bracket dom
mkExprBracket = mkAnn ("[|" <> child <> "|]") . UExprBracket

-- | Pattern bracket (@ [p| Point x y |] @)
mkPatternBracket :: Pattern dom -> Bracket dom
mkPatternBracket = mkAnn ("[p|" <> child <> "|]") . UPatternBracket

-- | Type bracket (@ [t| (Int,Int) |] @)
mkTypeBracket :: Type dom -> Bracket dom
mkTypeBracket = mkAnn ("[t|" <> child <> "|]") . UTypeBracket

-- | Declaration bracket (@ [d| f :: Int -> Int; f x = x*x |] @)
mkDeclsBracket :: [Decl dom] -> Bracket dom
mkDeclsBracket = mkAnn ("[d|" <> child <> "|]") . UDeclsBracket . mkAnnList indentedList

