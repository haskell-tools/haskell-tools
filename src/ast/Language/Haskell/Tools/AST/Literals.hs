-- | Haskell literals
module Language.Haskell.Tools.AST.Literals where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Haskell literals
data Literal a
  = CharLit       { _charLitValue :: Char } -- ^ character literal: @'c'@
  | StringLit     { _stringLitValue :: String }
  | IntLit        { _intLitValue :: Integer }
  | FracLit       { _fracLitValue :: Rational }
  | PrimIntLit    { _intLitValue :: Integer }
  | PrimFloatLit  { _floatLitValue :: Rational }
  | PrimDoubleLit { _floatLitValue :: Rational }
  | PrimCharLit   { _charLitValue :: Char }
  | PrimStringLit { _stringLitValue :: String }
               
-- | Literals promoted to kinds
data Promoted a
  = PromotedInt    { _promotedIntValue :: Integer }
  | PromotedString { _promotedStringValue :: String }
  | PromotedCon    { _promotedConName :: Name a }
  | PromotedList   { _promotedElements :: AnnList Promoted a }
  | PromotedTuple  { _promotedElements :: AnnList Promoted a }
  | PromotedUnit