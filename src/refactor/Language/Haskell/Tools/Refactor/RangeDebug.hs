{-# LANGUAGE TypeOperators
           , DefaultSignatures
           , StandaloneDeriving
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           #-}
module Language.Haskell.Tools.Refactor.RangeDebug where

import GHC.Generics
import SrcLoc
import Language.Haskell.Tools.AnnTrf.RangeToTemplate

rangeDebug :: TreeDebug e SrcSpan => e SrcSpan -> String
rangeDebug = treeDebug' shortShowSpan 0
      
shortShowSpan :: SrcSpan -> String
shortShowSpan (RealSrcSpan sp) 
  = show (srcSpanStartLine sp) ++ ":" ++ show (srcSpanStartCol sp) 
      ++ "-" ++ show (srcSpanEndLine sp) ++ ":" ++ show (srcSpanEndCol sp)
shortShowSpan _ = "???"
      
templateDebug :: TreeDebug e RangeTemplate => e RangeTemplate -> String
templateDebug = treeDebug' shortShowRangeTemplate 0

shortShowRangeTemplate (RangeTemplate _ rngs) = concatMap showRangeTemplateElem rngs
showRangeTemplateElem (RangeElem sp) = "[" ++ shortShowSpan sp ++ "]"
showRangeTemplateElem (RangeChildElem i) = "[" ++ show i ++ "]"
      
class TreeDebug e a where
  treeDebug' :: (a -> String) -> Int -> e a -> String
  default treeDebug' :: (GTreeDebug (Rep (e a)) a, Generic (e a)) => (a -> String) -> Int -> e a -> String
  treeDebug' f i = gTreeDebug f i . from

class GTreeDebug f a where 
  gTreeDebug :: (a -> String) -> Int -> f p -> String
  
instance GTreeDebug V1 a where
  gTreeDebug _ _ _ = undefined
  
instance GTreeDebug U1 a where
  gTreeDebug _ _ U1 = ""  
  
instance (GTreeDebug f a, GTreeDebug g a) => GTreeDebug (f :+: g) a where
  gTreeDebug f i (L1 x) = gTreeDebug f i x
  gTreeDebug f i (R1 x) = gTreeDebug f i x
  
instance (GTreeDebug f a, GTreeDebug g a) => GTreeDebug (f :*: g) a where
  gTreeDebug f i (x :*: y) 
    = gTreeDebug f i x ++ gTreeDebug f i y

instance {-# OVERLAPPING #-} TreeDebug e a => GTreeDebug (K1 i (e a)) a where
  gTreeDebug f i (K1 x) = treeDebug' f i x
  
instance {-# OVERLAPPABLE #-} GTreeDebug (K1 i c) a where
  gTreeDebug f i (K1 x) = ""
        
instance GTreeDebug f a => GTreeDebug (M1 i t f) a where
  gTreeDebug f i (M1 x) = gTreeDebug f i x
