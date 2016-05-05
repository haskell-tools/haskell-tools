{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module Language.Haskell.Tools.AST.Utils.GHCInstances where

import Data.Data
import SrcLoc

deriving instance Data SrcLoc
  
instance Data RealSrcLoc where
    gfoldl k z rsl = z mkRealSrcLoc `k` srcLocFile rsl `k` srcLocLine rsl `k` srcLocCol rsl

    gunfold k z c = case constrIndex c of
                        1 -> k (k (k (z mkRealSrcLoc)))

    toConstr _ = con_RSL
    dataTypeOf _ = ty_RSL

con_RSL = mkConstr ty_RSL "RSL" [] Prefix
ty_RSL   = mkDataType "SrcLoc.RealSrcLoc" [con_RSL]
