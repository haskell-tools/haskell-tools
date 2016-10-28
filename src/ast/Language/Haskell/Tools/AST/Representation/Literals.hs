-- | Representation of Haskell literals
module Language.Haskell.Tools.AST.Representation.Literals where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Names

-- | Haskell literals
data ULiteral dom stage
  = UCharLit { _charLitValue :: Char 
             } -- ^ Character literal: @'c'@
  | UStringLit { _stringLitValue :: String 
               } -- ^ String literal: @"abc"@
  | UIntLit { _intLitValue :: Integer 
            } -- ^ Integer literal: @12@
  | UFracLit { _fracLitValue :: Rational 
             } -- ^ Fractional literal: @3.14@
  | UPrimIntLit { _intLitValue :: Integer 
                } -- ^ Primitive integer literal (of type @Int#@): @32#@
  | UPrimWordLit { _intLitValue :: Integer 
                 } -- ^ Primitive word literal (of type @Word#@): @32##@
  | UPrimFloatLit { _floatLitValue :: Rational 
                  } -- ^ Primitive float literal (of type @Float#@): @3.14#@
  | UPrimDoubleLit { _floatLitValue :: Rational 
                   } -- ^ Primitive double literal (of type @Double#@): @3.14##@
  | UPrimCharLit { _charLitValue :: Char 
                 } -- ^ Primitive character literal (of type @Char#@): @'c'#@
  | UPrimStringLit { _stringLitValue :: String 
                   } -- ^ Primitive string literal (of type @Addr#@): @"xxx"#@
               