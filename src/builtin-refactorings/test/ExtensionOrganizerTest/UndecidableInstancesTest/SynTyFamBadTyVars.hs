{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module SynTyFamBadTyVars where

import Definitions hiding (CT)

type Syn0 a     = [a]
type Syn1 a b   = [(a,b)]
type Syn2 a     = (a,a)
type Syn3 a b c = (a,b,c)
type Syn4 a     = Syn3 a a a
type Syn5 a b   = (a,b)


type instance T1 (Syn0 a) = T1 (Syn2 a)   {-* UndecidableInstances, TypeFamilies *-}

type family CT a where
  CT (Syn1 a b)   = CT (Syn2 a)
  CT (Syn3 a b c) = CT (Syn4 a)         {-* UndecidableInstances, UndecidableInstances, TypeFamilies *-}

instance C (a,b) where
  type T (Syn5 a b) = T (Syn2 a)          {-* UndecidableInstances, TypeFamilies *-}
