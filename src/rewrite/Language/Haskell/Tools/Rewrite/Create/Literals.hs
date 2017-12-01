-- | Generation of literals for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.Haskell.Tools.Rewrite.Create.Literals where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST (ULiteral(..))
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn)
import Language.Haskell.Tools.Rewrite.ElementTypes (Literal)

-- | Character literal: @'c'@
mkCharLit :: Char -> Literal
mkCharLit c = mkAnn (fromString $ show c) $ UCharLit c

-- | String literal: @"abc"@
mkStringLit :: String -> Literal
mkStringLit s = mkAnn (fromString $ show s) $ UStringLit s

-- | Integer literal: @12@
mkIntLit :: Integer -> Literal
mkIntLit i = mkAnn (fromString $ show i) $ UIntLit i

-- | Fractional literal: @3.14@
mkFracLit :: Rational -> Literal
mkFracLit f = mkAnn (fromString $ show f) $ UFracLit f

-- | Primitive integer literal (of type @Int#@): @32#@
mkPrimIntLit :: Integer -> Literal
mkPrimIntLit i = mkAnn (fromString $ show i ++ "#") $ UPrimIntLit i

-- | Primitive word literal (of type @Word#@): @32##@
mkPrimWordLit :: Integer -> Literal
mkPrimWordLit i = mkAnn (fromString $ show i ++ "##") $ UPrimWordLit i

-- | Primitive float literal (of type @Float#@): @3.14#@
mkPrimFloatLit :: Rational -> Literal
mkPrimFloatLit f = mkAnn (fromString $ show f ++ "#") $ UPrimFloatLit f

-- | Primitive double literal (of type @Double#@): @3.14##@
mkPrimDoubleLit :: Rational -> Literal
mkPrimDoubleLit f = mkAnn (fromString $ show f ++ "##") $ UPrimDoubleLit f

-- | Primitive character literal (of type @Char#@): @'c'#@
mkPrimCharLit :: Char -> Literal
mkPrimCharLit c = mkAnn (fromString $ show c ++ "#") $ UPrimCharLit c

-- | Primitive string literal (of type @Addr#@): @"xxx"#@
mkPrimStringLit :: String -> Literal
mkPrimStringLit s = mkAnn (fromString $ show s ++ "#") $ UPrimStringLit s