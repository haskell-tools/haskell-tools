-- | Pattern matching on type-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms
           #-}
module Language.Haskell.Tools.AST.Match.Types where

import qualified Name as GHC
import Language.Haskell.Tools.AST

-- * Types

pattern TyForall :: AnnList TyVar dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyForall vars t <- Ann _ (UTyForall vars t)

pattern TyCtx :: Ann Context dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyCtx ctx t <- Ann _ (UTyCtx ctx t)

pattern TyFun :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyFun at rt <- Ann _ (UTyFun at rt)

pattern TyTuple :: AnnList Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyTuple args <- Ann _ (UTyTuple args)

pattern TyUnbTuple :: AnnList Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyUnbTuple args <- Ann _ (UTyUnbTuple args)

pattern TyList :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyList t <- Ann _ (UTyList t)

pattern TyParArray :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyParArray t <- Ann _ (UTyParArray t)

pattern TyApp :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyApp ft at <- Ann _ (UTyApp ft at)

pattern TyInfix :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyInfix left op right <- Ann _ (UTyInfix left op right)

pattern TyParen :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyParen t <- Ann _ (UTyParen t)

pattern TyVar :: Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyVar n <- Ann _ (UTyVar n)

pattern TyKinded :: Ann Type dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyKinded t k <- Ann _ (UTyKinded t k)

pattern TyBang :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyBang n <- Ann _ (UTyBang n)

pattern TyLazy :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyLazy n <- Ann _ (UTyLazy n)

pattern TyUnpack :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyUnpack n <- Ann _ (UTyUnpack n)

pattern TyWildcard :: Ann Type dom SrcTemplateStage
pattern TyWildcard <- Ann _ UTyWildcard

pattern TyNamedWildcard :: Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
pattern TyNamedWildcard n <- Ann _ (UTyNamedWildc n)

-- * Type variable

pattern TyVarDecl :: Ann Name dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage
pattern TyVarDecl n <- Ann _ (UTyVarDecl n _)

-- * Contexts

pattern ContextOne :: Ann Assertion dom SrcTemplateStage -> Ann Context dom SrcTemplateStage
pattern ContextOne n <- Ann _ (UContextOne n)

pattern ContextMulti :: AnnList Assertion dom SrcTemplateStage -> Ann Context dom SrcTemplateStage
pattern ContextMulti n <- Ann _ (UContextMulti n)

-- * Assertions

pattern ClassAssert :: Ann Name dom SrcTemplateStage -> AnnList Type dom SrcTemplateStage -> Ann Assertion dom SrcTemplateStage
pattern ClassAssert n args <- Ann _ (UClassAssert n args)

pattern InfixAssert :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Assertion dom SrcTemplateStage
pattern InfixAssert left op right <- Ann _ (UInfixAssert left op right)
