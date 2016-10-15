-- | Representation of Haskell types
module Language.Haskell.Tools.AST.Types where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Kinds
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

-- | Type variable declaration
data TyVar dom stage
  = UTyVarDecl { _tyVarName :: Ann Name dom stage
               , _tyVarKind :: AnnMaybe KindConstraint dom stage
               }

-- | Haskell types
data Type dom stage
  = UTyForall     { _typeBounded :: AnnList TyVar dom stage
                  , _typeType :: Ann Type dom stage
                  } -- ^ Forall types (@ forall x y . type @)
  | UTyCtx        { _typeCtx :: Ann Context dom stage
                  , _typeType :: Ann Type dom stage
                  } -- ^ Type with a context (@ forall x y . type @)
  | UTyFun        { _typeParam :: Ann Type dom stage
                  , _typeResult :: Ann Type dom stage
                  } -- ^ Function types (@ a -> b @)
  | UTyTuple      { _typeElements :: AnnList Type dom stage
                  } -- ^ Tuple types (@ (a,b) @)
  | UTyUnbTuple   { _typeElements :: AnnList Type dom stage
                  } -- ^ Unboxed tuple types (@ (#a,b#) @)
  | UTyList       { _typeElement :: Ann Type dom stage
                  } -- ^ List type with special syntax (@ [a] @)
  | UTyParArray   { _typeElement :: Ann Type dom stage
                  } -- ^ Parallel array type (@ [:a:] @)
  | UTyApp        { _typeCon :: Ann Type dom stage
                  , _typeArg :: Ann Type dom stage
                  } -- ^ Type application (@ F a @)
  | UTyVar        { _typeName :: Ann Name dom stage
                  } -- ^ type variable or constructor (@ a @)
  | UTyParen      { _typeInner :: Ann Type dom stage
                  } -- ^ type surrounded by parentheses (@ (T a) @)
  | UTyInfix      { _typeLeft :: Ann Type dom stage
                  , _typeOperator :: Ann Operator dom stage
                  , _typeRight :: Ann Type dom stage
                  } -- ^ Infix type constructor (@ (a <: b) @)
  | UTyKinded     { _typeInner :: Ann Type dom stage
                  , _typeKind :: Ann Kind dom stage
                  } -- ^ Type with explicit kind signature (@ _a :: * @)
  | UTyPromoted   { _tpPromoted :: Ann (Promoted Type) dom stage
                  } -- A promoted data type with @-XDataKinds@ (@ 3 @, @ Left @, @ 'Left @).
  | UTySplice     { _tsSplice :: Splice dom stage
                  } -- ^ a Template Haskell splice type (@ $(genType) @).
  | UTyQuasiQuote { _typeQQ :: QuasiQuote dom stage
                  } -- ^ a Template Haskell quasi-quote type (@ [quoter| ... ] @).
  | UTyBang       { _typeInner :: Ann Type dom stage
                  } -- ^ Strict type marked with @!@.
  | UTyLazy       { _typeInner :: Ann Type dom stage
                  } -- ^ Lazy type marked with @~@. (Should only be used if @Strict@ or @StrictData@ language extension is used)
  | UTyUnpack     { _typeInner :: Ann Type dom stage
                  } -- ^ Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
  | UTyNoUnpack   { _typeInner :: Ann Type dom stage
                  } -- ^ Strict type marked with NOUNPACK pragma. (Usually contains the bang mark.)
  | UTyWildcard   -- ^ A wildcard type (@ _ @) with @-XPartialTypeSignatures@
  | UTyNamedWildc { _typeWildcardName :: Ann Name dom stage
                  } -- ^ A named wildcard type (@ _t @) with @-XPartialTypeSignatures@

-- One or more assertions
data Context dom stage
  = UContextOne   { _contextAssertion :: Ann Assertion dom stage
                  } -- ^ One assertion (@ C a => ... @)
  | UContextMulti { _contextAssertions :: AnnList Assertion dom stage
                  } -- ^ A set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)

-- | A single assertion in the context
data Assertion dom stage
  = UClassAssert { _assertClsName :: Ann Name dom stage
                 , _assertTypes :: AnnList Type dom stage
                 } -- ^ Class assertion (@Cls x@)
  | UInfixAssert { _assertLhs :: Ann Type dom stage
                 , _assertOp :: Ann Operator dom stage
                 , _assertRhs :: Ann Type dom stage
                 } -- ^ Infix class assertion, also contains type equations (@ a ~ X y @)
  | UImplicitAssert { _assertImplVar :: Ann Name dom stage
                    , _assertImplType :: Ann Type dom stage
                    } -- ^ Assertion for implicit parameter binding (@ ?cmp :: a -> a -> Bool @)
                 
                 