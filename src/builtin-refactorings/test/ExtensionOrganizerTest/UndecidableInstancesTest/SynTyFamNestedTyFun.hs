{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module SynTyFamNestedTyFun where

import Definitions hiding (CT)

type Syn1 a = T1 (T1 a)
type Syn2 a = CT (CT a)

type instance T1 [a] = Syn1 a  {-* UndecidableInstances, TypeFamilies *-}

type family CT a where
  CT [[a]] = Syn2 a
  CT a     = Syn2 a            {-* UndecidableInstances, UndecidableInstances, TypeFamilies *-}

instance C [a] where
  type T [a] = Syn1 a          {-* UndecidableInstances, TypeFamilies *-}
