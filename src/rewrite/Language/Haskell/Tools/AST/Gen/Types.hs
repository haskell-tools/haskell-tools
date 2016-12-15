-- | Generation of type-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTyForall@ creates the annotated version of the @TyForall@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Types where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AST.Gen.Kinds
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.Transform

-- * Generation of types

-- | Forall types (@ forall x y . type @)
mkForallType :: [TyVar dom] -> Type dom -> Type dom
mkForallType vars t = mkAnn ("forall " <> child <> " . " <> child) (UTyForall (mkAnnList (separatedBy " " list) vars) t)

-- | Simplified creation of type variables
mkTypeVar' :: GHC.Name -> TyVar dom
mkTypeVar' = mkTypeVar . mkUnqualName'

-- | Type with a context (@ forall x y . type @)
mkCtxType :: Context dom -> Type dom -> Type dom
mkCtxType ctx t = mkAnn (child <> " " <> child) (UTyCtx ctx t)

-- | Function types (@ a -> b @)
mkFunctionType :: Type dom -> Type dom -> Type dom
mkFunctionType at rt = mkAnn (child <> " -> " <> child) (UTyFun at rt)

-- | Tuple types (@ (a,b) @)
mkTupleType :: [Type dom] -> Type dom
mkTupleType args = mkAnn ("(" <> child <> ")") (UTyTuple (mkAnnList (separatedBy ", " list) args))

-- | Unboxed tuple types (@ (\#a,b\#) @)
mkUnboxedTupleType :: [Type dom] -> Type dom
mkUnboxedTupleType args = mkAnn ("(#" <> child <> "#)") (UTyUnbTuple (mkAnnList (separatedBy ", " list) args))

-- | List type with special syntax (@ [a] @)
mkListType :: Type dom -> Type dom
mkListType = mkAnn ("[" <> child <> "]") . UTyList

-- | Parallel array type (@ [:a:] @)
mkParArrayType :: Type dom -> Type dom
mkParArrayType = mkAnn ("[:" <> child <> ":]") . UTyParArray

-- | Type application (@ F a @)
mkTypeApp :: Type dom -> Type dom -> Type dom
mkTypeApp ft at = mkAnn (child <> " " <> child) (UTyApp ft at)

-- | Infix type constructor (@ (a <: b) @)
mkInfixTypeApp :: Type dom -> Operator dom -> Type dom -> Type dom
mkInfixTypeApp left op right = mkAnn (child <> " " <> child <> " " <> child) (UTyInfix left op right)
             
-- | Type surrounded by parentheses (@ (T a) @)
mkParenType :: Type dom -> Type dom
mkParenType = mkAnn ("(" <> child <> ")") . UTyParen

-- | Creates a simple type variable
mkTypeVar :: Name dom -> TyVar dom
mkTypeVar n = mkAnn (child <> child) (UTyVarDecl n noth)

-- | Creates a type variable with kind specification (@ t :: * @)
mkKindedTypeVar :: Name dom -> Kind dom -> TyVar dom
mkKindedTypeVar n k = mkAnn (child <> child) (UTyVarDecl n (justVal (mkKindConstraint k)))

-- | Type variable or constructor (@ a @)
mkVarType :: Name dom -> Type dom
mkVarType = wrapperAnn . UTyVar

-- | Type with explicit kind signature (@ a :: * @)
mkKindedType :: Type dom -> Kind dom -> Type dom
mkKindedType t k = mkAnn (child <> " :: " <> child) (UTyKinded t k)

-- | Strict type marked with @!@.
mkBangType :: Type dom -> Type dom
mkBangType = mkAnn ("!" <> child) . UTyBang

-- | Lazy type marked with @~@. (Should only be used if @Strict@ or @StrictData@ language extension is used)
mkLazyType :: Type dom -> Type dom
mkLazyType = mkAnn ("~" <> child) . UTyLazy

-- | Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
mkUnpackType :: Type dom -> Type dom
mkUnpackType = mkAnn ("{-# UNPACK #-} " <> child) . UTyUnpack

-- | Strict type marked with UNPACK pragma. (Usually contains the bang mark.)
mkNoUnpackType :: Type dom -> Type dom
mkNoUnpackType = mkAnn ("{-# NOUNPACK #-} " <> child) . UTyNoUnpack

-- | A wildcard type (@ _ @) with @-XPartialTypeSignatures@
mkWildcardType :: Type dom
mkWildcardType = mkAnn "_" UTyWildcard

-- | A named wildcard type (@ _t @) with @-XPartialTypeSignatures@
mkNamedWildcardType :: Name dom -> Type dom
mkNamedWildcardType = mkAnn ("_" <> child) . UTyNamedWildc

-- | A Template Haskell splice type (@ $(genType) @).
mkSpliceType :: Splice dom -> Type dom
mkSpliceType = mkAnn child . UTySplice

-- | A Template Haskell quasi-quote type (@ [quoter| ... ] @).
mkQuasiQuoteType :: QuasiQuote dom -> Type dom
mkQuasiQuoteType = mkAnn child . UTyQuasiQuote


-- | Numeric value promoted to the kind level.
mkPromotedIntType :: Integer -> Type dom
mkPromotedIntType i = mkAnn child $ UTyPromoted $ mkAnn (fromString $ show i) (UPromotedInt i)

-- | String value promoted to the kind level.
mkPromotedStringType :: String -> Type dom
mkPromotedStringType i = mkAnn child $ UTyPromoted $ mkAnn (fromString $ show i) (UPromotedString i)

-- | A data constructor value promoted to the kind level.
mkPromotedConType :: Name dom -> Type dom
mkPromotedConType = mkAnn child . UTyPromoted . mkAnn child . UPromotedCon

-- | A list of elements as a kind.
mkPromotedListType :: [Type dom] -> Type dom
mkPromotedListType 
  = mkAnn child . UTyPromoted . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (separatedBy ", " list)

-- | A tuple of elements as a kind.
mkPromotedTupleType :: [Type dom] -> Type dom
mkPromotedTupleType 
  = mkAnn child . UTyPromoted . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (separatedBy ", " list)

-- | Kind of the unit value @()@. 
mkPromotedUnitType :: Type dom
mkPromotedUnitType = mkAnn child $ UTyPromoted $ mkAnn "()" UPromotedUnit

-- * Generation of contexts

-- | Creates a context of one assertion (@ C a => ... @)
mkContextOne :: Assertion dom -> Context dom
mkContextOne = mkAnn (child <> " =>") . UContextOne

-- | Creates a context of a set of assertions (@ (C1 a, C2 b) => ... @, but can be one: @ (C a) => ... @)
mkContextMulti :: [Assertion dom] -> Context dom
mkContextMulti = mkAnn ("(" <> child <> ") =>") . UContextMulti . mkAnnList (separatedBy ", " list)

-- * Generation of assertions

-- | Class assertion (@Cls x@)
mkClassAssert :: Name dom -> [Type dom] -> Assertion dom
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ UClassAssert n (mkAnnList (separatedBy " " list) args)

-- | Infix class assertion, also contains type equations (@ a ~ X y @)
mkInfixAssert :: Type dom -> Operator dom -> Type dom -> Assertion dom
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAssert left op right

-- | Creates an assertion for implicit parameter binding (@ ?cmp :: a -> a -> Bool @)
mkImplicitAssert :: Name dom -> Type dom -> Assertion dom
mkImplicitAssert n t = mkAnn (child <> " :: " <> child) $ UImplicitAssert n t

