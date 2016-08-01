-- | Representation of Haskell literals
module Language.Haskell.Tools.AST.Literals where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Haskell literals
data Literal dom stage
  = CharLit { _charLitValue :: Char 
            } -- ^ Character literal: @'c'@
  | StringLit { _stringLitValue :: String 
              } -- ^ String literal: @"abc"@
  | IntLit { _intLitValue :: Integer 
           } -- ^ Integer literal: @12@
  | FracLit { _fracLitValue :: Rational 
            } -- ^ Fractional literal: @3.14@
  | PrimIntLit { _intLitValue :: Integer 
               } -- ^ Primitive integer literal (of type @Int#@): @32#@
  | PrimWordLit { _intLitValue :: Integer 
                } -- ^ Primitive word literal (of type @Word#@): @32##@
  | PrimFloatLit { _floatLitValue :: Rational 
                 } -- ^ Primitive float literal (of type @Float#@): @3.14#@
  | PrimDoubleLit { _floatLitValue :: Rational 
                  } -- ^ Primitive double literal (of type @Double#@): @3.14##@
  | PrimCharLit { _charLitValue :: Char 
                } -- ^ Primitive character literal (of type @Char#@): @'c'#@
  | PrimStringLit { _stringLitValue :: String }
               