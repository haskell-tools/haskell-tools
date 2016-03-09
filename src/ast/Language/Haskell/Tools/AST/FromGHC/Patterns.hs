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
import Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST.Ann
import qualified Language.Haskell.Tools.AST.Patterns as AST

trfPattern :: TransformName n r => Located (Pat n) -> Trf (Ann AST.Pattern r)
trfPattern = trfLoc trfPattern'

trfPattern' :: TransformName n r => Pat n -> Trf (AST.Pattern r)
trfPattern' (WildPat _) = pure AST.WildPat
trfPattern' (VarPat name) = AST.VarPat <$> trfNameSp' name
trfPattern' (LazyPat pat) = AST.IrrPat <$> trfPattern pat
trfPattern' (AsPat name pat) = AST.AsPat <$> trfName name <*> trfPattern pat
trfPattern' (ParPat pat) = AST.ParenPat <$> trfPattern pat
trfPattern' (BangPat pat) = AST.BangPat <$> trfPattern pat
trfPattern' (ListPat pats _ _) = AST.ListPat <$> trfAnnList trfPattern' pats
trfPattern' (TuplePat pats Boxed _) = AST.TuplePat <$> trfAnnList trfPattern' pats
trfPattern' (PArrPat pats _) = AST.ParArrPat <$> trfAnnList trfPattern' pats
trfPattern' (ConPatIn name _) = AST.VarPat <$> trfNameSp' (unLoc name)
trfPattern' (ViewPat expr pat _) = AST.ViewPat <$> trfExpr expr <*> trfPattern pat
trfPattern' (SplicePat splice) = AST.SplicePat <$> annCont (trfSplice' splice)
trfPattern' (QuasiQuotePat qq) = AST.QuasiQuotePat <$> annCont (trfQuasiQuotation' qq)
trfPattern' (LitPat lit) = AST.LitPat <$> annCont (trfLiteral' lit)
trfPattern' (NPat (ol_val . unLoc -> lit) _ _) = AST.LitPat <$> annCont (trfOverloadedLit lit)
trfPattern' (SigPatIn pat (hswb_cts -> typ)) = AST.TypeSigPat <$> trfPattern pat <*> trfType typ
  -- NPlusKPat, CoPat?