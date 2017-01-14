-- | Generation of literals for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Literals where

import Data.String (IsString(..), String(..))
import Language.Haskell.Tools.AST (ULiteral(..))
import Language.Haskell.Tools.AST.ElementTypes (Literal(..))
import Language.Haskell.Tools.AST.Gen.Utils (mkAnn)

-- | Character literal: @'c'@
mkCharLit :: Char -> Literal dom
mkCharLit c = mkAnn (fromString $ show c) $ UCharLit c

-- | String literal: @"abc"@
mkStringLit :: String -> Literal dom
mkStringLit s = mkAnn (fromString $ show s) $ UStringLit s

-- | Integer literal: @12@
mkIntLit :: Integer -> Literal dom
mkIntLit i = mkAnn (fromString $ show i) $ UIntLit i

-- | Fractional literal: @3.14@
mkFracLit :: Rational -> Literal dom
mkFracLit f = mkAnn (fromString $ show f) $ UFracLit f

-- | Primitive integer literal (of type @Int#@): @32#@
mkPrimIntLit :: Integer -> Literal dom
mkPrimIntLit i = mkAnn (fromString $ show i ++ "#") $ UPrimIntLit i

-- | Primitive word literal (of type @Word#@): @32##@
mkPrimWordLit :: Integer -> Literal dom
mkPrimWordLit i = mkAnn (fromString $ show i ++ "##") $ UPrimWordLit i

-- | Primitive float literal (of type @Float#@): @3.14#@
mkPrimFloatLit :: Rational -> Literal dom
mkPrimFloatLit f = mkAnn (fromString $ show f ++ "#") $ UPrimFloatLit f

-- | Primitive double literal (of type @Double#@): @3.14##@
mkPrimDoubleLit :: Rational -> Literal dom
mkPrimDoubleLit f = mkAnn (fromString $ show f ++ "##") $ UPrimDoubleLit f

-- | Primitive character literal (of type @Char#@): @'c'#@
mkPrimCharLit :: Char -> Literal dom
mkPrimCharLit c = mkAnn (fromString $ show c ++ "#") $ UPrimCharLit c

-- | Primitive string literal (of type @Addr#@): @"xxx"#@
mkPrimStringLit :: String -> Literal dom
mkPrimStringLit s = mkAnn (fromString $ show s ++ "#") $ UPrimStringLit s