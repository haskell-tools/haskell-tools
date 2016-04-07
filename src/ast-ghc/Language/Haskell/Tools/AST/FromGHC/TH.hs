module Language.Haskell.Tools.AST.FromGHC.TH where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsExpr as GHC
import ApiAnnotation as GHC
import FastString as GHC

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Patterns
import Language.Haskell.Tools.AST.FromGHC.Base

import qualified Language.Haskell.Tools.AST as AST

trfQuasiQuotation' :: TransformName n r => HsQuasiQuote n -> Trf (AST.QuasiQuote r)
trfQuasiQuotation' (HsQuasiQuote id l str) = AST.QuasiQuote <$> between AnnOpenS AnnVbar (annCont (trfName' id)) 
                                                            <*> annLoc (pure l) (pure $ AST.QQString (unpackFS str))

trfSplice' :: TransformName n r => HsSplice n -> Trf (AST.Splice r)
trfSplice' (HsSplice _ expr) = AST.ParenSplice <$> trfExpr expr

trfBracket' :: TransformName n r => HsBracket n -> Trf (AST.Bracket r)
trfBracket' (ExpBr expr) = AST.ExprBracket <$> trfExpr expr
trfBracket' (TExpBr expr) = AST.ExprBracket <$> trfExpr expr
trfBracket' (VarBr _ expr) = error "VarBr" -- AST.ExprBracket <$> copyAnnot AST.Var (trfName expr)
trfBracket' (PatBr pat) = AST.PatternBracket <$> trfPattern pat
trfBracket' (DecBrL decls) = AST.DeclsBracket <$> trfDecls decls
trfBracket' (DecBrG decls) = AST.DeclsBracket <$> trfDeclsGroup decls
trfBracket' (TypBr typ) = AST.TypeBracket <$> trfType typ
