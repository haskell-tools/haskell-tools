-- | Generation of type-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTyForall@ creates the annotated version of the @TyForall@ AST constructor.
{-# LANGUAGE MonoLocalBinds, OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.Types where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Kinds (mkKindConstraint)
import Language.Haskell.Tools.Rewrite.Create.Names (mkUnqualName')
import Language.Haskell.Tools.Rewrite.Create.Utils
import Language.Haskell.Tools.Rewrite.ElementTypes
import qualified Name as GHC (Name)

-- * Generation of types

-- | Forall types (@ forall x y . type @)
mkForallType :: [TyVar] -> Type -> Type
mkForallType vars t = mkAnn ("forall " <> child <> " . " <> child) (UTyForall (mkAnnList (separatedBy " " list) vars) t)

-- | Simplified creation of type variables
mkTypeVar' :: GHC.Name -> TyVar
mkTypeVar' = mkTypeVar . mkUnqualName'

-- | Type with a context (@ forall x y . type @)
mkCtxType :: Context -> Type -> Type
mkCtxType ctx t = mkAnn (child <> " " <> child) (UTyCtx ctx t)

-- | Function types (@ a -> b @)
mkFunctionType :: Type -> Type -> Type
mkFunctionType at rt = mkAnn (child <> " -> " <> child) (UTyFun at rt)

-- | Tuple types (@ (a,b) @)
mkTupleType :: [Type] -> Type
mkTupleType args = mkAnn ("(" <> child <> ")") (UTyTuple (mkAnnList (separatedBy ", " list) args))

-- | Unboxed tuple types (@ (\#a,b\#) @)
mkUnboxedTupleType :: [Type] -> Type
mkUnboxedTupleType args = mkAnn ("(#" <> child <> "#)") (UTyUnbTuple (mkAnnList (separatedBy ", " list) args))

-- | List type with special syntax (@ [a] @)
mkListType :: Type -> Type
mkListType = mkAnn ("[" <> child <> "]") . UTyList

-- | Parallel array type (@ [:a:] @)
mkParArrayType :: Type -> Type
mkParArrayType = mkAnn ("[:" <> child <> ":]") . UTyParArray

-- | Type application (@ F a @)
mkTypeApp :: Type -> Type -> Type
mkTypeApp ft at = mkAnn (child <> " " <> child) (UTyApp ft at)

-- | Infix type constructor (@ (a <: b) @)
mkInfixTypeApp :: Type -> Operator -> Type -> Type
mkInfixTypeApp left op right = mkAnn (child <> " " <> child <> " " <> child) (UTyInfix left op right)

-- | Type surrounded by parentheses (@ (T a) @)
mkParenType :: Type -> Type
mkParenType = mkAnn ("(" <> child <> ")") . UTyParen

-- | Creates a simple type variable
mkTypeVar :: Name -> TyVar
mkTypeVar n = mkAnn (child <> child) (UTyVarDecl n noth)

-- | Creates a type variable with kind specification (@ t :: * @)
mkKindedTypeVar :: Name -> Kind -> TyVar
mkKindedTypeVar n k = mkAnn (child <> child) (UTyVarDecl n (justVal (mkKindConstraint k)))

-- | Type variable or constructor (@ a @)
mkVarType :: Name -> Type
mkVarType = wrapperAnn . UTyVar

-- | Type with explicit kind signature (@ a :: * @)
mkKindedType :: Type -> Kind -> Type
mkKindedType t k = mkAnn (child <> " :: " <> child) (UTyKinded t k)

-- | Strict type marked with @!@.
mkBangType :: Type -> Type
mkBangType = mkAnn ("!" <> child) . UTyBang

-- | Lazy type marked with @~@. (Should only be used if @Strict@ or @StrictData@ language extension is used)
mkLazyType :: Type -> Type
mkLazyType = mkAnn ("~" <> child) . UTyLazy

-- | Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
mkUnpackType :: Type -> Type
mkUnpackType = mkAnn ("{-# UNPACK #-} " <> child) . UTyUnpack

-- | Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
mkNoUnpackType :: Type -> Type
mkNoUnpackType = mkAnn ("{-# NOUNPACK #-} " <> child) . UTyNoUnpack

-- | A wildcard type (@ _ @) with @-XPartialTypeSignatures@
mkWildcardType :: Type
mkWildcardType = mkAnn "_" UTyWildcard

-- | A named wildcard type (@ _t @) with @-XPartialTypeSignatures@
mkNamedWildcardType :: Name -> Type
mkNamedWildcardType = mkAnn ("_" <> child) . UTyNamedWildc

-- | A Template Haskell splice type (@ $(genType) @).
mkSpliceType :: Splice -> Type
mkSpliceType = mkAnn child . UTySplice

-- | A Template Haskell quasi-quote type (@ [quoter| ... ] @).
mkQuasiQuoteType :: QuasiQuote -> Type
mkQuasiQuoteType = mkAnn child . UTyQuasiQuote


-- | Numeric value promoted to the kind level.
mkPromotedIntType :: Integer -> Type
mkPromotedIntType i = mkAnn child $ UTyPromoted $ mkAnn (fromString $ show i) (UPromotedInt i)

-- | String value promoted to the kind level.
mkPromotedStringType :: String -> Type
mkPromotedStringType i = mkAnn child $ UTyPromoted $ mkAnn (fromString $ show i) (UPromotedString i)

-- | A data constructor value promoted to the kind level.
mkPromotedConType :: Name -> Type
mkPromotedConType = mkAnn child . UTyPromoted . mkAnn child . UPromotedCon

-- | A list of elements as a kind.
mkPromotedListType :: [Type] -> Type
mkPromotedListType
  = mkAnn child . UTyPromoted . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (separatedBy ", " list)

-- | A tuple of elements as a kind.
mkPromotedTupleType :: [Type] -> Type
mkPromotedTupleType
  = mkAnn child . UTyPromoted . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (separatedBy ", " list)

-- | Kind of the unit value @()@.
mkPromotedUnitType :: Type
mkPromotedUnitType = mkAnn child $ UTyPromoted $ mkAnn "()" UPromotedUnit

-- * Generation of contexts

-- | Creates a context of assertions (@ C a => ... @)
mkContext :: Assertion -> Context
mkContext = mkAnn (child <> " =>") . UContext

-- * Generation of assertions

-- | Class assertion (@Cls x@)
mkClassAssert :: Name -> [Type] -> Assertion
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ UClassAssert n (mkAnnList (separatedBy " " list) args)

-- | Infix class assertion, also contains type equations (@ a ~ X y @)
mkInfixAssert :: Type -> Operator -> Type -> Assertion
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAssert left op right

-- | Creates an assertion for implicit parameter binding (@ ?cmp :: a -> a -> Bool @)
mkImplicitAssert :: Name -> Type -> Assertion
mkImplicitAssert n t = mkAnn (child <> " :: " <> child) $ UImplicitAssert n t

-- | Creates a list of assertions (@ (Eq a, Show a) @)
mkTupleAssertion :: [Assertion] -> Assertion
mkTupleAssertion ass = mkAnn ("(" <> child <> ")") $ UTupleAssert $ mkAnnList (separatedBy ", " list) ass
