-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Base where

import Language.Haskell.Tools.AST

pattern NormalOp :: Ann QualifiedName dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage
pattern NormalOp n <- Ann _ (UNormalOp n)

pattern BacktickOp :: Ann QualifiedName dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage
pattern BacktickOp n <- Ann _ (UBacktickOp n)

pattern NormalName :: Ann QualifiedName dom SrcTemplateStage -> Ann Name dom SrcTemplateStage
pattern NormalName n <- Ann _ (UNormalName n)

pattern ParenName :: Ann QualifiedName dom SrcTemplateStage -> Ann Name dom SrcTemplateStage
pattern ParenName n <- Ann _ (UParenName n)

pattern NamePart :: String -> Ann NamePart dom SrcTemplateStage
pattern NamePart s <- Ann _ (UNamePart s)

pattern StringNode :: String -> Ann StringNode dom SrcTemplateStage
pattern StringNode s <- Ann _ (UStringNode s )

pattern ModuleName :: String -> Ann ModuleName dom SrcTemplateStage
pattern ModuleName s <- Ann _ (UModuleName s)

pattern DataKeyword :: Ann DataOrNewtypeKeyword dom SrcTemplateStage
pattern DataKeyword <- Ann _ UDataKeyword

pattern NewtypeKeyword :: Ann DataOrNewtypeKeyword dom SrcTemplateStage
pattern NewtypeKeyword <- Ann _ UNewtypeKeyword

pattern DoKeyword :: Ann DoKind dom SrcTemplateStage
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: Ann DoKind dom SrcTemplateStage
pattern MDoKeyword <- Ann _ UMDoKeyword