module Language.Haskell.Tools.AST.FromGHC.TH where

import Control.Monad.Reader

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsExpr as GHC
import ApiAnnotation as GHC
import FastString as GHC
import OccName as GHC
import SrcLoc as GHC

import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils
import Language.Haskell.Tools.AST.FromGHC.Decls
import Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Types
import Language.Haskell.Tools.AST.FromGHC.Patterns
import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

import Language.Haskell.Tools.AST (Ann, Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfQuasiQuotation' :: TransformName n r => HsSplice n -> Trf (AST.QuasiQuote (Dom r) RangeStage)
 -- the lexer does not provide us with tokens '[', '|' and '|]'
trfQuasiQuotation' (HsQuasiQuote id _ l str) 
  = AST.QuasiQuote <$> annLocNoSema (pure quoterLoc) (trfName' id)
                   <*> annLocNoSema (pure strLoc) (pure $ AST.QQString (unpackFS str))
  where quoterLoc = mkSrcSpan (updateCol (subtract (1 + length (occNameString $ rdrNameOcc $ rdrName id))) (srcSpanStart l)) 
                              (updateCol (subtract 1) (srcSpanStart l))
        strLoc = mkSrcSpan (srcSpanStart l) (updateCol (subtract 2) (srcSpanEnd l))

trfSplice :: TransformName n r => Located (HsSplice n) -> Trf (Ann AST.Splice (Dom r) RangeStage)
trfSplice = trfLocNoSema trfSplice'

trfSplice' :: TransformName n r => HsSplice n -> Trf (AST.Splice (Dom r) RangeStage)
trfSplice' (HsTypedSplice _ expr) = AST.ParenSplice <$> trfCorrectDollar expr
trfSplice' (HsUntypedSplice _ expr) = AST.ParenSplice <$> trfCorrectDollar expr

trfCorrectDollar :: TransformName n r => Located (HsExpr n) -> Trf (Ann AST.Expr (Dom r) RangeStage)
trfCorrectDollar expr = 
  do isSplice <- allTokenLoc AnnThIdSplice
     case isSplice of [] -> trfExpr expr
                      _  -> let newSp = updateStart (updateCol (+1)) (getLoc expr) 
                             in case expr of L _ (HsVar (L _ varName)) -> trfExpr $ L newSp (HsVar (L newSp varName))
                                             L _ exp                   -> trfExpr $ L newSp exp

trfBracket' :: TransformName n r => HsBracket n -> Trf (AST.Bracket (Dom r) RangeStage)
trfBracket' (ExpBr expr) = AST.ExprBracket <$> trfExpr expr
trfBracket' (TExpBr expr) = AST.ExprBracket <$> trfExpr expr
trfBracket' (VarBr isSingle expr) 
  = AST.ExprBracket <$> annLoc createScopeInfo (updateStart (updateCol (if isSingle then (+1) else (+2))) <$> asks contRange) 
      (AST.Var <$> (annContNoSema (trfName' expr)))
trfBracket' (PatBr pat) = AST.PatternBracket <$> trfPattern pat
trfBracket' (DecBrL decls) = AST.DeclsBracket <$> trfDecls decls
trfBracket' (DecBrG decls) = AST.DeclsBracket <$> trfDeclsGroup decls
trfBracket' (TypBr typ) = AST.TypeBracket <$> trfType typ
