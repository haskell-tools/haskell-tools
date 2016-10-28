-- | UPattern matching on type-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms
           #-}
module Language.Haskell.Tools.AST.Match.Types where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- * Types

-- | Forall types (@ forall x y . type @)
pattern ForallType :: TyVarList dom -> Type dom -> Type dom
pattern ForallType vars t <- Ann _ (UTyForall vars t)

-- | Type with a context (@ forall x y . type @)
pattern CtxType :: Context dom -> Type dom -> Type dom
pattern CtxType ctx t <- Ann _ (UTyCtx ctx t)

-- | Function types (@ a -> b @)
pattern FunctionType :: Type dom -> Type dom -> Type dom
pattern FunctionType at rt <- Ann _ (UTyFun at rt)

-- | Tuple types (@ (a,b) @)
pattern TupleType :: TypeList dom -> Type dom
pattern TupleType args <- Ann _ (UTyTuple args)

-- | Unboxed tuple types (@ (#a,b#) @)
pattern UnboxedTupleType :: TypeList dom -> Type dom
pattern UnboxedTupleType args <- Ann _ (UTyUnbTuple args)

-- | List type with special syntax (@ [a] @)
pattern ListType :: Type dom -> Type dom
pattern ListType t <- Ann _ (UTyList t)

-- | Parallel array type (@ [:a:] @)
pattern ParArrayType :: Type dom -> Type dom
pattern ParArrayType t <- Ann _ (UTyParArray t)

-- | Type application (@ F a @)
pattern TypeApp :: Type dom -> Type dom -> Type dom
pattern TypeApp ft at <- Ann _ (UTyApp ft at)

-- | Infix type constructor (@ (a <: b) @)
pattern InfixTypeApp :: Type dom -> Operator dom -> Type dom -> Type dom
pattern InfixTypeApp left op right <- Ann _ (UTyInfix left op right)

-- | Type surrounded by parentheses (@ (T a) @)
pattern ParenType :: Type dom -> Type dom
pattern ParenType t <- Ann _ (UTyParen t)

-- | Type variable or constructor (@ a @)
pattern VarType :: Name dom -> Type dom
pattern VarType n <- Ann _ (UTyVar n)

-- | Type with explicit kind signature (@ a :: * @)
pattern KindedType :: Type dom -> Kind dom -> Type dom
pattern KindedType t k <- Ann _ (UTyKinded t k)

-- | Strict type marked with @!@.
pattern BangType :: Type dom -> Type dom
pattern BangType n <- Ann _ (UTyBang n)

-- | Lazy type marked with @~@. (Should only be used if @Strict@ or @StrictData@ language extension is used)
pattern LazyType :: Type dom -> Type dom
pattern LazyType n <- Ann _ (UTyLazy n)

-- | Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
pattern UnpackType :: Type dom -> Type dom
pattern UnpackType n <- Ann _ (UTyUnpack n)

-- | Strict type marked with NOUNPACK pragma. (Usually contains the bang mark.)
pattern NoUnpackType :: Type dom -> Type dom
pattern NoUnpackType t <- Ann _ (UTyNoUnpack t)

-- | A wildcard type (@ _ @) with @-XPartialTypeSignatures@
pattern WildcardType :: Type dom
pattern WildcardType <- Ann _ UTyWildcard

-- | A named wildcard type (@ _t @) with @-XPartialTypeSignatures@
pattern NamedWildcardType :: Name dom -> Type dom
pattern NamedWildcardType n <- Ann _ (UTyNamedWildc n)

-- | A Template Haskell splice type (@ $(genType) @).
pattern SpliceType :: Splice dom -> Type dom
pattern SpliceType spl <- Ann _ (UTySplice spl)

-- | A Template Haskell splice type (@ $(genType) @).
pattern QuasiQuoteType :: QuasiQuote dom -> Type dom
pattern QuasiQuoteType spl <- Ann _ (UTyQuasiQuote spl)

-- | Numeric value promoted to the type level.
pattern PromotedIntType :: Integer -> Type dom
pattern PromotedIntType i <- Ann _ (UTyPromoted (Ann _ (UPromotedInt i)))

-- | String value promoted to the type level.
pattern PromotedStringType :: String -> Type dom
pattern PromotedStringType s <- Ann _ (UTyPromoted (Ann _ (UPromotedString s)))

-- | A data constructor value promoted to the type level.
pattern PromotedConType :: Name dom -> Type dom
pattern PromotedConType s <- Ann _ (UTyPromoted (Ann _ (UPromotedCon s)))

-- | A list of elements as a type.
pattern PromotedListType :: TypeList dom -> Type dom
pattern PromotedListType elems <- Ann _ (UTyPromoted (Ann _ (UPromotedList elems)))

-- | A tuple of elements as a type.
pattern PromotedTupleType :: TypeList dom -> Type dom
pattern PromotedTupleType elems <- Ann _ (UTyPromoted (Ann _ (UPromotedTuple elems)))

-- | Kind of the unit value @()@. 
pattern PromotedUnitType :: Type dom
pattern PromotedUnitType <- Ann _ (UTyPromoted (Ann _ UPromotedUnit))

-- * Type variable

-- | Type variable declaration
pattern TyVarDecl :: Name dom -> TyVar dom
pattern TyVarDecl n <- Ann _ (UTyVarDecl n _)

-- * Contexts

-- | One assertion (@ C a => ... @)
pattern ContextOne :: Assertion dom -> Context dom
pattern ContextOne n <- Ann _ (UContextOne n)

-- | A set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)
pattern ContextMulti :: AssertionList dom -> Context dom
pattern ContextMulti n <- Ann _ (UContextMulti n)

-- * Assertions

-- | Class assertion (@Cls x@)
pattern ClassAssert :: Name dom -> TypeList dom -> Assertion dom
pattern ClassAssert n args <- Ann _ (UClassAssert n args)

-- | Infix class assertion, also contains type equations (@ a ~ X y @)
pattern InfixAssert :: Type dom -> Operator dom -> Type dom -> Assertion dom
pattern InfixAssert left op right <- Ann _ (UInfixAssert left op right)

-- | Assertion for implicit parameter binding (@ ?cmp :: a -> a -> Bool @)
pattern ImplicitAssert :: Name dom -> Type dom -> Assertion dom
pattern ImplicitAssert n t <- Ann _ (UImplicitAssert n t)
