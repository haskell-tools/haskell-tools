{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, MultiParamTypeClasses, FlexibleInstances, PolyKinds, UndecidableInstances, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, FlexibleContexts #-}

module Control.Instances.TypeLevelPrelude where

import GHC.TypeLits

type family Const a b where
  Const a b = a
  
type family Seq a b where
  Seq a b = b
  
type family LazyIfThenElse p a b where
  LazyIfThenElse True a b = a
  LazyIfThenElse False a b = b
  
type family Iterate a where
  Iterate a = a ': Iterate a

type family l1 :++: l2 where
  '[] :++: l2       = l2
  (e ': r1) :++: l2 = e ': (r1 :++: l2)
  
type family Elem e ls where
  Elem e '[] = False
  Elem e (e ': ls) = True
  Elem e (x ': ls) = Elem e ls
  
type family IfThenElse (b :: Bool) (th :: x) (el :: x) :: x where
  IfThenElse True  th el = th
  IfThenElse False th el = el
         
type family Length ls :: Nat where
  Length '[] = 0
  Length (e ': ls) = 1 + Length ls
           
type family Head (ls :: [k]) :: k where
  Head (e ': ls) = e   
  
type family HeadMaybe (ls :: [k]) :: Maybe k where
  HeadMaybe (e ': ls) = Just e
  HeadMaybe '[] = Nothing
  
type family FromMaybe d m where
  FromMaybe d (Just x) = x
  FromMaybe d Nothing = d
  
type family Same a b where
  Same a a = True
  Same a b = False
  
type family Null (ls :: [k]) :: Bool where
  Null '[] = True
  Null ls = False
           
type family MapAppend e lls where
  MapAppend e (ls ': lls) = (e ': ls) ': MapAppend e lls
  MapAppend e '[] = '[]
  
type family AppendJust m ls where
  AppendJust (Just x) ls = x ': ls
  AppendJust Nothing ls = ls
  
type family Revert ls where
  Revert '[] = '[]
  Revert (e ': ls) = Revert ls :++: '[ e ]
  
type family IfThenJust (p :: Bool) (v :: k) :: Maybe k where
  IfThenJust True v = Just v
  IfThenJust False v = Nothing  
  
type family IfJust (p :: Maybe k) (t :: kr) (e :: kr) :: kr where
  IfJust (Just x) t e = t
  IfJust Nothing t e = e
  
type family IsJust (p :: Maybe k) :: Bool where
  IsJust (Just x) = True
  IsJust Nothing = False    
  
type family FromJust (p :: Maybe k) :: k where
  FromJust (Just x) = x
  
type family Cat (f2 :: k2 -> k3) (f1 :: k1 -> k2) (a :: k1) :: k3 where
  Cat f2 f1 a = f2 (f1 a)
  

  
  