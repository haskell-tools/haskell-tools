{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, MultiParamTypeClasses, FlexibleInstances, PolyKinds, UndecidableInstances, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, FlexibleContexts, OverlappingInstances #-}

module Control.Instances.ShortestPath where

import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.State
import Data.Maybe
import Data.Proxy
import GHC.TypeLits
import Control.Instances.TypeLevelPrelude

-- * Generic datatypes to store connections
  
data Connect m1 m2
data Connect_2m m1 m2
data Connect_mt mt
data Connect_id m
data Connect_MU m

-- | Marks that there is no legal path between the two types according
-- to the rulebase.
data NoPathFound
  
type family ShortestPath (e :: [*]) (s :: * -> *) (t :: * -> *) :: [*] where
  ShortestPath e t t = '[]
  ShortestPath e Identity t = '[ Connect_id t ]
  ShortestPath e s Proxy = '[ Connect_MU s ]
  ShortestPath e s t = ShortestPath' e s (InitCurrent e t)
  
type family ShortestPath' (e :: [*]) (s :: * -> *) (c :: [[*]]) :: [*] where
  ShortestPath' e s '[] = '[ NoPathFound ]
  ShortestPath' e s c = FromMaybe (ShortestPath' e s (ApplyEdges e c c))
                                  (GetFinished s c) 
                                  
                                      
type family GetFinished s c where
  GetFinished s ((Connect s b ': p) ': lls) 
    = Just (Connect s b ': p)
  GetFinished s (p ': lls) = GetFinished s lls
  GetFinished s '[] = Nothing

type family InitCurrent (e :: [*]) (t :: * -> *) :: [[*]] where
  InitCurrent '[] t = '[]
  InitCurrent (e ': es) t = IfJust (ApplyEdge e t)
                                   ('[ MonomorphEnd e t ] ': InitCurrent es t) 
                                   (InitCurrent es t)
  
type family ApplyEdges (e :: [*]) (co :: [[*]]) (c :: [[*]]) :: [[*]] where
  ApplyEdges (e ': es) co ((Connect s b ': p) ': cs) 
    = AppendJust (IfThenJust (IsJust (ApplyEdge e s)) 
                                (MonomorphEnd e s ': Connect s b ': p)) 
                 (ApplyEdges (e ': es) co cs)
  ApplyEdges (e ': es) co '[] = ApplyEdges es co co
  ApplyEdges '[] co cr = '[]  
  
type family ApplyEdge e t :: Maybe (* -> *) where
  ApplyEdge (Connect ms mr) mr = Just ms
  ApplyEdge (Connect_2m ms mt) (mt m) = Just ms
  ApplyEdge (Connect_mt mt) (mt m) = Just m
  ApplyEdge e t = Nothing

type family MonomorphEnd c v :: * where
  MonomorphEnd (Connect_2m m m') v = Connect m v
  MonomorphEnd (Connect_mt t) (t m) = Connect m (t m)
  MonomorphEnd c v = c


