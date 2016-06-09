-- | Representation of Haskell types
module Language.Haskell.Tools.AST.Types where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Kinds
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

-- | Type variable declaration
data TyVar a 
  = TyVarDecl { _tyVarName :: Ann Name a
              , _tyVarKind :: AnnMaybe KindConstraint a
              }

-- | Haskell types
data Type a
  = TyForall     { _typeBounded :: AnnList TyVar a
                 , _typeType :: Ann Type a
                 } -- ^ Forall types (@ forall x y . type @)
  | TyCtx        { _typeCtx :: Ann Context a
                 , _typeType :: Ann Type a
                 } -- ^ Type with a context (@ forall x y . type @)
  | TyFun        { _typeParam :: Ann Type a
                 , _typeResult :: Ann Type a
                 } -- ^ Function types (@ a -> b @)
  | TyTuple      { _typeElements :: AnnList Type a
                 } -- ^ Tuple types (@ (a,b) @)
  | TyUnbTuple   { _typeElements :: AnnList Type a 
                 } -- ^ Unboxed tuple types (@ (#a,b#) @)
  | TyList       { _typeElement :: Ann Type a 
                 } -- ^ List type with special syntax (@ [a] @)
  | TyParArray   { _typeElement :: Ann Type a
                 } -- ^ Parallel array type (@ [:a:] @)
  | TyApp        { _typeCon :: Ann Type a
                 , _typeArg :: Ann Type a
                 } -- ^ Type application (@ F a @)
  | TyVar        { _typeName :: Ann Name a
                 } -- ^ type variable or constructor (@ a @)
  | TyParen      { _typeInner :: Ann Type a
                 } -- ^ type surrounded by parentheses (@ (T a) @)
  | TyInfix      { _typeLeft :: Ann Type a 
                 , _typeOperator :: Ann Operator a
                 , _typeRight :: Ann Type a
                 } -- ^ Infix type constructor (@ (a <: b) @)
  | TyKinded     { _typeInner :: Ann Type a
                 , _typeKind :: Ann Kind a
                 } -- ^ Type with explicit kind signature (@ _a :: * @)
  | TyPromoted   { _tpPromoted :: Ann (Promoted Type) a
                 } -- A promoted data type with @-XDataKinds@ (@ 3 @, @ Left @, @ 'Left @).
  | TySplice     { _tsSplice :: Splice a
                 } -- ^ a Template Haskell splice type (@ $(genType) @).
  | TyQuasiQuote { _typeQQ :: QuasiQuote a
                 } -- ^ a Template Haskell quasi-quote type (@ [quoter| ... ] @).
  | TyBang       { _typeInner :: Ann Type a
                 } -- ^ Strict type marked with "!".
  | TyUnpack     { _typeInner :: Ann Type a
                 } -- ^ Type marked with UNPACK pragma.
  | TyNoUnpack   { _typeInner :: Ann Type a
                 } -- ^ Type marked with NOUNPACK pragma.
  | TyWildcard   -- ^ A wildcard type (@ _ @) with @-XPartialTypeSignatures@
  | TyNamedWildc { _typeWildcardName :: Ann Name a
                 } -- ^ A named wildcard type (@ _t @) with @-XPartialTypeSignatures@

-- One or more assertions
data Context a
  = ContextOne   { _contextAssertion :: Ann Assertion a
                 } -- ^ One assertion (@ C a => ... @)
  | ContextMulti { _contextAssertions :: AnnList Assertion a 
                 } -- ^ A set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)

-- | A single assertion in the context
data Assertion a
  = ClassAssert { _assertClsName :: Ann Name a
                , _assertTypes :: AnnList Type a
                } -- ^ Class assertion (@Cls x@)
  | InfixAssert { _assertLhs :: Ann Type a
                , _assertOp :: Ann Operator a
                , _assertRhs :: Ann Type a
                } -- ^ Infix class assertion, also contains type equations (@ a ~ X y @)
                 
                 