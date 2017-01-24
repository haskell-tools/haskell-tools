-- | UPattern matching on literals for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Literals where

import Language.Haskell.Tools.AST (ULiteral(..), Ann(..))
import Language.Haskell.Tools.AST.ElementTypes (Literal)

-- | Character literal: @'c'@
pattern CharLit :: Char -> Literal dom
pattern CharLit c <- Ann _ (UCharLit c)

-- | String literal: @"abc"@
pattern StringLit :: String -> Literal dom
pattern StringLit s <- Ann _ (UStringLit s)

-- | Integer literal: @12@
pattern IntLit :: Integer -> Literal dom
pattern IntLit i <- Ann _ (UIntLit i)

-- | Fractional literal: @3.14@
pattern FracLit :: Rational -> Literal dom
pattern FracLit f <- Ann _ (UFracLit f)

-- | Primitive integer literal (of type @Int#@): @32#@
pattern PrimIntLit :: Integer -> Literal dom
pattern PrimIntLit i <- Ann _ (UPrimIntLit i)

-- | Primitive word literal (of type @Word#@): @32##@
pattern PrimWordLit :: Integer -> Literal dom
pattern PrimWordLit i <- Ann _ (UPrimWordLit i)

-- | Primitive float literal (of type @Float#@): @3.14#@
pattern PrimFloatLit :: Rational -> Literal dom
pattern PrimFloatLit i <- Ann _ (UPrimFloatLit i)

-- | Primitive double literal (of type @Double#@): @3.14##@
pattern PrimDoubleLit :: Rational -> Literal dom
pattern PrimDoubleLit i <- Ann _ (UPrimDoubleLit i)

-- | Primitive character literal (of type @Char#@): @'c'#@
pattern PrimCharLit :: Char -> Literal dom
pattern PrimCharLit i <- Ann _ (UPrimCharLit i)

-- | Primitive string literal (of type @Addr#@): @"xxx"#@
pattern PrimStringLit :: String -> Literal dom
pattern PrimStringLit s <- Ann _ (UPrimStringLit s)
