-- | UPattern matching on type-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.Rewrite.Match.Types where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite.ElementTypes

-- * Types

-- | Forall types (@ forall x y . type @)
pattern ForallType :: TyVarList -> Type -> Type
pattern ForallType vars t <- Ann _ (UTyForall vars t)

-- | Type with a context (@ forall x y . type @)
pattern CtxType :: Context -> Type -> Type
pattern CtxType ctx t <- Ann _ (UTyCtx ctx t)

-- | Function types (@ a -> b @)
pattern FunctionType :: Type -> Type -> Type
pattern FunctionType at rt <- Ann _ (UTyFun at rt)

-- | Tuple types (@ (a,b) @)
pattern TupleType :: TypeList -> Type
pattern TupleType args <- Ann _ (UTyTuple args)

-- | Unboxed tuple types (@ (\#a,b\#) @)
pattern UnboxedTupleType :: TypeList -> Type
pattern UnboxedTupleType args <- Ann _ (UTyUnbTuple args)

-- | List type with special syntax (@ [a] @)
pattern ListType :: Type -> Type
pattern ListType t <- Ann _ (UTyList t)

-- | Parallel array type (@ [:a:] @)
pattern ParArrayType :: Type -> Type
pattern ParArrayType t <- Ann _ (UTyParArray t)

-- | Type application (@ F a @)
pattern TypeApp :: Type -> Type -> Type
pattern TypeApp ft at <- Ann _ (UTyApp ft at)

-- | Infix type constructor (@ (a <: b) @)
pattern InfixTypeApp :: Type -> Operator -> Type -> Type
pattern InfixTypeApp left op right <- Ann _ (UTyInfix left op right)

-- | Type surrounded by parentheses (@ (T a) @)
pattern ParenType :: Type -> Type
pattern ParenType t <- Ann _ (UTyParen t)

-- | Type variable or constructor (@ a @)
pattern VarType :: Name -> Type
pattern VarType n <- Ann _ (UTyVar n)

-- | Type with explicit kind signature (@ a :: * @)
pattern KindedType :: Type -> Kind -> Type
pattern KindedType t k <- Ann _ (UTyKinded t k)

-- | Strict type marked with @!@.
pattern BangType :: Type -> Type
pattern BangType n <- Ann _ (UTyBang n)

-- | Lazy type marked with @~@. (Should only be used if @Strict@ or @StrictData@ language extension is used)
pattern LazyType :: Type -> Type
pattern LazyType n <- Ann _ (UTyLazy n)

-- | Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
pattern UnpackType :: Type -> Type
pattern UnpackType n <- Ann _ (UTyUnpack n)

-- | Strict type marked with NOUNPACK pragma. (Usually contains the bang mark.)
pattern NoUnpackType :: Type -> Type
pattern NoUnpackType t <- Ann _ (UTyNoUnpack t)

-- | A wildcard type (@ _ @) with @-XPartialTypeSignatures@
pattern WildcardType :: Type
pattern WildcardType <- Ann _ UTyWildcard

-- | A named wildcard type (@ _t @) with @-XPartialTypeSignatures@
pattern NamedWildcardType :: Name -> Type
pattern NamedWildcardType n <- Ann _ (UTyNamedWildc n)

-- | A Template Haskell splice type (@ $(genType) @).
pattern SpliceType :: Splice -> Type
pattern SpliceType spl <- Ann _ (UTySplice spl)

-- | A Template Haskell splice type (@ $(genType) @).
pattern QuasiQuoteType :: QuasiQuote -> Type
pattern QuasiQuoteType spl <- Ann _ (UTyQuasiQuote spl)

-- | Numeric value promoted to the type level.
pattern PromotedIntType :: Integer -> Type
pattern PromotedIntType i <- Ann _ (UTyPromoted (Ann _ (UPromotedInt i)))

-- | String value promoted to the type level.
pattern PromotedStringType :: String -> Type
pattern PromotedStringType s <- Ann _ (UTyPromoted (Ann _ (UPromotedString s)))

-- | A data constructor value promoted to the type level.
pattern PromotedConType :: Name -> Type
pattern PromotedConType s <- Ann _ (UTyPromoted (Ann _ (UPromotedCon s)))

-- | A list of elements as a type.
pattern PromotedListType :: TypeList -> Type
pattern PromotedListType elems <- Ann _ (UTyPromoted (Ann _ (UPromotedList elems)))

-- | A tuple of elements as a type.
pattern PromotedTupleType :: TypeList -> Type
pattern PromotedTupleType elems <- Ann _ (UTyPromoted (Ann _ (UPromotedTuple elems)))

-- | Kind of the unit value @()@.
pattern PromotedUnitType :: Type
pattern PromotedUnitType <- Ann _ (UTyPromoted (Ann _ UPromotedUnit))

-- * Type variable

-- | Type variable declaration
pattern TyVarDecl :: Name -> TyVar
pattern TyVarDecl n <- Ann _ (UTyVarDecl n _)

-- | Kinded type variable declaration (@ v :: * @)
pattern KindedTyVarDecl :: Name -> Kind -> TyVar
pattern KindedTyVarDecl n k <- Ann _ (UTyVarDecl n (AnnJust (Ann _ (UKindConstraint k))))

-- * Contexts

-- | A context of assertions (@ C a => ... @)
pattern Context :: Assertion -> Context
pattern Context n <- Ann _ (UContext n)

-- * Assertions

-- | Class assertion (@Cls x@)
pattern ClassAssert :: Name -> TypeList -> Assertion
pattern ClassAssert n args <- Ann _ (UClassAssert n args)

-- | Infix class assertion, also contains type equations (@ a ~ X y @)
pattern InfixAssert :: Type -> Operator -> Type -> Assertion
pattern InfixAssert left op right <- Ann _ (UInfixAssert left op right)

-- | Assertion for implicit parameter binding (@ ?cmp :: a -> a -> Bool @)
pattern ImplicitAssert :: Name -> Type -> Assertion
pattern ImplicitAssert n t <- Ann _ (UImplicitAssert n t)

-- | A list of assertions (@ (Eq a, Show a) @)
pattern TupleAssert :: [Assertion] -> Assertion
pattern TupleAssert ass <- Ann _ (UTupleAssert (AnnList ass))
