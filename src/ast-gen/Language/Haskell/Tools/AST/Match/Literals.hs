-- | Pattern matching on literals for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Literals where

import Language.Haskell.Tools.AST

pattern CharLit :: Char -> Ann Literal dom SrcTemplateStage
pattern CharLit c <- Ann _ (UCharLit c)

pattern StringLit :: String -> Ann Literal dom SrcTemplateStage
pattern StringLit s <- Ann _ (UStringLit s)

pattern IntLit :: Integer -> Ann Literal dom SrcTemplateStage
pattern IntLit i <- Ann _ (UIntLit i)

pattern FracLit :: Rational -> Ann Literal dom SrcTemplateStage
pattern FracLit f <- Ann _ (UFracLit f)

pattern PrimIntLit :: Integer -> Ann Literal dom SrcTemplateStage
pattern PrimIntLit i <- Ann _ (UPrimIntLit i)

pattern PrimWordLit :: Integer -> Ann Literal dom SrcTemplateStage
pattern PrimWordLit i <- Ann _ (UPrimWordLit i)

pattern PrimFloatLit :: Rational -> Ann Literal dom SrcTemplateStage
pattern PrimFloatLit i <- Ann _ (UPrimFloatLit i)

pattern PrimDoubleLit :: Rational -> Ann Literal dom SrcTemplateStage
pattern PrimDoubleLit i <- Ann _ (UPrimDoubleLit i)

pattern PrimCharLit :: Char -> Ann Literal dom SrcTemplateStage
pattern PrimCharLit i <- Ann _ (UPrimCharLit i)

pattern PrimStringLit :: String -> Ann Literal dom SrcTemplateStage
pattern PrimStringLit s <- Ann _ (UPrimStringLit s)
