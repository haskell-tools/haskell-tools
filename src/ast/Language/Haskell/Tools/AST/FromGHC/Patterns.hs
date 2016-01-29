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
trfPattern = trfLoc $ \case
  WildPat _ -> pure AST.WildPat
  VarPat name -> AST.VarPat <$> annCont (trfName' name)
  LazyPat pat -> AST.IrrPat <$> trfPattern pat
  AsPat name pat -> AST.AsPat <$> trfName name <*> trfPattern pat
  ParPat pat -> AST.ParenPat <$> trfPattern pat
  BangPat pat -> AST.BangPat <$> trfPattern pat
  ListPat pats _ _ -> AST.ListPat . AnnList <$> mapM trfPattern pats
  TuplePat pats Boxed _ -> AST.TuplePat . AnnList <$> mapM trfPattern pats
  PArrPat pats _ -> AST.ParArrPat . AnnList <$> mapM trfPattern pats
  ConPatIn name _ -> AST.VarPat <$> annCont (trfName' (unLoc name))
  ViewPat expr pat _ -> AST.ViewPat <$> trfExpr expr <*> trfPattern pat
  SplicePat splice -> AST.SplicePat <$> annCont (trfSplice' splice)
  QuasiQuotePat qq -> AST.QuasiQuotePat <$> annCont (trfQuasiQuotation' qq)
  LitPat lit -> AST.LitPat <$> annCont (trfLiteral' lit)
  NPat (ol_val . unLoc -> lit) _ _ -> AST.LitPat <$> annCont (trfOverloadedLit lit)
  SigPatIn pat (hswb_cts -> typ) -> AST.TypeSigPat <$> trfPattern pat <*> trfType typ
  -- NPlusKPat, CoPat?