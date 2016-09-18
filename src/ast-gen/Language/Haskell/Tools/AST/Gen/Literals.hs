-- | Generation of literals for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Literals where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkCharLit :: Char -> Ann Literal dom SrcTemplateStage
mkCharLit c = mkAnn (fromString $ show c) $ CharLit c

mkStringLit :: String -> Ann Literal dom SrcTemplateStage
mkStringLit s = mkAnn (fromString $ show s) $ StringLit s

mkIntLit :: Integer -> Ann Literal dom SrcTemplateStage
mkIntLit i = mkAnn (fromString $ show i) $ IntLit i

mkFracLit :: Rational -> Ann Literal dom SrcTemplateStage
mkFracLit f = mkAnn (fromString $ show f) $ FracLit f

mkPrimIntLit :: Integer -> Ann Literal dom SrcTemplateStage
mkPrimIntLit i = mkAnn (fromString $ show i ++ "#") $ PrimIntLit i

mkPrimWordLit :: Integer -> Ann Literal dom SrcTemplateStage
mkPrimWordLit i = mkAnn (fromString $ show i ++ "##") $ PrimWordLit i

mkPrimFloatLit :: Rational -> Ann Literal dom SrcTemplateStage
mkPrimFloatLit f = mkAnn (fromString $ show f ++ "#") $ PrimFloatLit f

mkPrimDoubleLit :: Rational -> Ann Literal dom SrcTemplateStage
mkPrimDoubleLit f = mkAnn (fromString $ show f ++ "##") $ PrimDoubleLit f

mkPrimCharLit :: Char -> Ann Literal dom SrcTemplateStage
mkPrimCharLit c = mkAnn (fromString $ show c ++ "#") $ PrimCharLit c

mkPrimStringLit :: String -> Ann Literal dom SrcTemplateStage
mkPrimStringLit s = mkAnn (fromString $ show s ++ "#") $ PrimStringLit s