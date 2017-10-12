-- | Representation of Haskell types
module Language.Haskell.Tools.AST.Representation.Types where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG, AnnMaybeG)
import Language.Haskell.Tools.AST.Representation.Kinds (UPromoted, UKind, UKindConstraint)
import Language.Haskell.Tools.AST.Representation.Names (UName, UOperator)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.TH (UQuasiQuote, USplice)

-- | Type variable declaration
data UTyVar dom stage
  = UTyVarDecl { _tyVarName :: Ann UName dom stage
               , _tyVarKind :: AnnMaybeG UKindConstraint dom stage
               }

-- | Haskell types
data UType dom stage
  = UTyForall     { _typeBounded :: AnnListG UTyVar dom stage
                  , _typeType :: Ann UType dom stage
                  } -- ^ Forall types (@ forall x y . type @)
  | UTyCtx        { _typeCtx :: Ann UContext dom stage
                  , _typeType :: Ann UType dom stage
                  } -- ^ Type with a context (@ forall x y . type @)
  | UTyFun        { _typeParam :: Ann UType dom stage
                  , _typeResult :: Ann UType dom stage
                  } -- ^ Function types (@ a -> b @)
  | UTyTuple      { _typeElements :: AnnListG UType dom stage
                  } -- ^ Tuple types (@ (a,b) @)
  | UTyUnbTuple   { _typeElements :: AnnListG UType dom stage
                  } -- ^ Unboxed tuple types (@ (#a,b#) @)
  | UTyList       { _typeElement :: Ann UType dom stage
                  } -- ^ List type with special syntax (@ [a] @)
  | UTyParArray   { _typeElement :: Ann UType dom stage
                  } -- ^ Parallel array type (@ [:a:] @)
  | UTyApp        { _typeCon :: Ann UType dom stage
                  , _typeArg :: Ann UType dom stage
                  } -- ^ Type application (@ F a @)
  | UTyVar        { _typeName :: Ann UName dom stage
                  } -- ^ Type variable or constructor (@ a @)
  | UTyParen      { _typeInner :: Ann UType dom stage
                  } -- ^ Type surrounded by parentheses (@ (T a) @)
  | UTyInfix      { _typeLeft :: Ann UType dom stage
                  , _typeOperator :: Ann UOperator dom stage
                  , _typeRight :: Ann UType dom stage
                  } -- ^ Infix type constructor (@ (a <: b) @)
  | UTyKinded     { _typeInner :: Ann UType dom stage
                  , _typeKind :: Ann UKind dom stage
                  } -- ^ Type with explicit kind signature (@ a :: * @)
  | UTyPromoted   { _tpPromoted :: Ann (UPromoted UType) dom stage
                  } -- A promoted data type with @-XDataKinds@ (@ 3 @, @ Left @, @ 'Left @).
  | UTySplice     { _tsSplice :: Ann USplice dom stage
                  } -- ^ A Template Haskell splice type (@ $(genType) @).
  | UTyQuasiQuote { _typeQQ :: Ann UQuasiQuote dom stage
                  } -- ^ A Template Haskell quasi-quote type (@ [quoter| ... ] @).
  | UTyBang       { _typeInner :: Ann UType dom stage
                  } -- ^ Strict type marked with @!@.
  | UTyLazy       { _typeInner :: Ann UType dom stage
                  } -- ^ Lazy type marked with @~@. (Should only be used if @Strict@ or @StrictData@ language extension is used)
  | UTyUnpack     { _typeInner :: Ann UType dom stage
                  } -- ^ Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
  | UTyNoUnpack   { _typeInner :: Ann UType dom stage
                  } -- ^ Strict type marked with NOUNPACK pragma. (Usually contains the bang mark.)
  | UTyWildcard   -- ^ A wildcard type (@ _ @) with @-XPartialTypeSignatures@
  | UTyNamedWildc { _typeWildcardName :: Ann UName dom stage
                  } -- ^ A named wildcard type (@ _t @) with @-XPartialTypeSignatures@
  | UUnbSumType   { _typeElements :: AnnListG UType dom stage
                  } -- ^ An unboxed sum type (@ (# Bool | Int | String #) @)

-- One or more assertions
data UContext dom stage
  = UContext { _contextAssertion :: Ann UAssertion dom stage
             } -- ^ Assertions with the fat arrow (@ C a => ... @)

-- | A single assertion in the context
data UAssertion dom stage
  = UClassAssert { _assertClsName :: Ann UName dom stage
                 , _assertTypes :: AnnListG UType dom stage
                 } -- ^ Class assertion (@Cls x@)
  | UInfixAssert { _assertLhs :: Ann UType dom stage
                 , _assertOp :: Ann UOperator dom stage
                 , _assertRhs :: Ann UType dom stage
                 } -- ^ Infix class assertion, also contains type equations (@ a ~ X y @)
  | UImplicitAssert { _assertImplVar :: Ann UName dom stage
                    , _assertImplType :: Ann UType dom stage
                    } -- ^ Assertion for implicit parameter binding (@ ?cmp :: a -> a -> Bool @)
  | UTupleAssert { _innerAsserts :: AnnListG UAssertion dom stage
                 } -- ^ Multiple assertions in one (@ (Ord a, Show a) @)
  | UWildcardAssert -- ^ Wildcard assertion (@ _ @), enabled by @PartialTypeSignatures@
