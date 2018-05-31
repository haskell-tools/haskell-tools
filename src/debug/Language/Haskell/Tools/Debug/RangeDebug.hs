{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | A module for displaying debug info about the source annotations of the syntax tree in different phases.
module Language.Haskell.Tools.Debug.RangeDebug where

import Control.Reference ()
import GHC.Generics
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.BackendGHC ()
import Language.Haskell.Tools.PrettyPrint.Prepare ()
import Language.Haskell.Tools.Debug.Show ()

type ShowSrcInfo st = (Show (SpanInfo st), Show (ListInfo st), Show (OptionalInfo st)) 

srcInfoDebug :: TreeDebug e dom st => e dom st -> String
srcInfoDebug = treeDebug' 0

class (ShowSrcInfo st, Domain dom, Show (e dom st))
        => TreeDebug e dom st where
  treeDebug' :: Int -> e dom st -> String
  default treeDebug' :: (GTreeDebug (Rep (e dom st)), Generic (e dom st), Domain dom) => Int -> e dom st -> String
  treeDebug' i = gTreeDebug i . from

class GTreeDebug f where
  gTreeDebug :: Int -> f p -> String

instance GTreeDebug V1 where
  gTreeDebug _ = error "GTreeDebug V1"

instance GTreeDebug U1 where
  gTreeDebug _ U1 = ""

instance (GTreeDebug f, GTreeDebug g) => GTreeDebug (f :+: g) where
  gTreeDebug i (L1 x) = gTreeDebug i x
  gTreeDebug i (R1 x) = gTreeDebug i x

instance (GTreeDebug f, GTreeDebug g) => GTreeDebug (f :*: g) where
  gTreeDebug i (x :*: y) = gTreeDebug i x ++ gTreeDebug i y

instance {-# OVERLAPPING #-} TreeDebug e dom st => GTreeDebug (K1 i (e dom st)) where
  gTreeDebug i (K1 x) = treeDebug' i x

instance {-# OVERLAPPABLE #-} GTreeDebug (K1 i c) where
  gTreeDebug _ (K1 _) = ""

instance GTreeDebug f => GTreeDebug (M1 i t f) where
  gTreeDebug i (M1 x) = gTreeDebug i x
