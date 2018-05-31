{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module SynTyFamNoSmaller where

import Definitions hiding (CT)

type Syn1 a = T1 a
type Syn2 a = CT [a]
type Syn3 a = CT a
type Syn4 a = T [[a]]

type instance T1 a = Syn1 a         {-* UndecidableInstances, TypeFamilies *-}

type family CT a where
  CT [a] = Syn2 a
  CT a   = Syn3 a                   {-* UndecidableInstances, UndecidableInstances, TypeFamilies *-}

instance C [a] where
  type T [a] = Syn4 a            {-* UndecidableInstances, TypeFamilies *-}
