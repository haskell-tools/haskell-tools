{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module TyFamBadTyVars where

import Definitions hiding (CT)

type instance T1 [a] = T1 (a,a)   {-* UndecidableInstances, TypeFamilies *-}

type family CT a where
  CT [(a,b)] = CT (a,a)
  CT (a,b,c) = CT (a,a,a)         {-* UndecidableInstances, UndecidableInstances, TypeFamilies *-}

instance C (a,b) where
  type T (a,b) = T (a,a)          {-* UndecidableInstances, TypeFamilies *-}
