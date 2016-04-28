{-# LANGUAGE LambdaCase
           , ViewPatterns
           #-}
module Language.Haskell.Tools.AST.FromGHC.Patterns where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsPat as GHC
import HsLit as GHC
import ApiAnnotation as GHC
import BasicTypes as GHC

import Language.Haskell.Tools.AST.FromGHC.Base
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..))
import qualified Language.Haskell.Tools.AST as AST

trfPattern :: TransformName n r => Located (Pat n) -> Trf (Ann AST.Pattern r)
-- field wildcards are not directly represented in GHC AST
trfPattern (L l (ConPatIn name (RecCon (HsRecFields flds _)))) | any ((l ==) . getLoc) flds 
  = do normalFields <- mapM (trfLoc trfPatternField') (filter ((l /=) . getLoc) flds)
       wildc <- annLoc (tokenLoc AnnDotdot) (pure AST.FieldWildcardPattern)
       annLoc (pure l) (AST.RecPat <$> trfName name <*> makeNonemptyList ", " (pure (normalFields ++ [wildc])))
trfPattern p | otherwise = trfLoc trfPattern' p

trfPattern' :: TransformName n r => Pat n -> Trf (AST.Pattern r)
trfPattern' (WildPat _) = pure AST.WildPat
trfPattern' (VarPat name) = define $ AST.VarPat <$> trfNameSp' name
trfPattern' (LazyPat pat) = AST.IrrPat <$> trfPattern pat
trfPattern' (AsPat name pat) = AST.AsPat <$> define (trfName name) <*> trfPattern pat
trfPattern' (ParPat pat) = AST.ParenPat <$> trfPattern pat
trfPattern' (BangPat pat) = AST.BangPat <$> trfPattern pat
trfPattern' (ListPat pats _ _) = AST.ListPat <$> trfAnnList ", " trfPattern' pats
trfPattern' (TuplePat pats Boxed _) = AST.TuplePat <$> trfAnnList ", " trfPattern' pats
trfPattern' (PArrPat pats _) = AST.ParArrPat <$> trfAnnList ", " trfPattern' pats
trfPattern' (ConPatIn name (PrefixCon args)) = AST.AppPat <$> trfName name <*> trfAnnList " " trfPattern' args
trfPattern' (ConPatIn name (RecCon (HsRecFields flds _))) = AST.RecPat <$> trfName name <*> trfAnnList ", " trfPatternField' flds
trfPattern' (ConPatIn name (InfixCon left right)) = AST.InfixPat <$> trfPattern left <*> trfName name <*> trfPattern right
trfPattern' (ViewPat expr pat _) = AST.ViewPat <$> trfExpr expr <*> trfPattern pat
trfPattern' (SplicePat splice) = AST.SplicePat <$> annCont (trfSplice' splice)
trfPattern' (QuasiQuotePat qq) = AST.QuasiQuotePat <$> annCont (trfQuasiQuotation' qq)
trfPattern' (LitPat lit) = AST.LitPat <$> annCont (trfLiteral' lit)
trfPattern' (SigPatIn pat (hswb_cts -> typ)) = AST.TypeSigPat <$> trfPattern pat <*> trfType typ
trfPattern' (NPat (ol_val . unLoc -> lit) _ _) = AST.LitPat <$> annCont (trfOverloadedLit lit)
trfPattern' (NPlusKPat id (L l lit) _ _) = AST.NPlusKPat <$> define (trfName id) <*> annLoc (pure l) (trfOverloadedLit (ol_val lit))
-- coercion pattern introduced by GHC
trfPattern' (CoPat _ pat _) = trfPattern' pat

trfPatternField' :: TransformName n r => HsRecField n (LPat n) -> Trf (AST.PatternField r)
trfPatternField' (HsRecField id arg False) = AST.NormalFieldPattern <$> trfName id <*> trfPattern arg
trfPatternField' (HsRecField id _ True) = AST.FieldPunPattern <$> trfName id


