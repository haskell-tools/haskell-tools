-- | Haskell literals
module Language.Haskell.Tools.AST.Literals where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Haskell literals
data Literal a
  = CharLit       { charLitValue :: Char } -- ^ character literal: @'c'@
  | StringLit     { stringLitValue :: String }
  | IntLit        { intLitValue :: Integer }
  | FracLit       { fracLitValue :: Rational }
  | PrimIntLit    { intLitValue :: Integer }
  | PrimFloatLit  { floatLitValue :: Rational }
  | PrimDoubleLit { floatLitValue :: Rational }
  | PrimCharLit   { charLitValue :: Char }
  | PrimStringLit { stringLitValue :: String }
               
-- | Literals promoted to kinds
data Promoted a
  = PromotedInt    { promotedIntValue :: Integer }
  | PromotedString { promotedStringValue :: String }
  | PromotedCon    { promotedConName :: Name a }
  | PromotedList   { promotedElements :: AnnList Promoted a }
  | PromotedTuple  { promotedElements :: AnnList Promoted a }
  | PromotedUnit