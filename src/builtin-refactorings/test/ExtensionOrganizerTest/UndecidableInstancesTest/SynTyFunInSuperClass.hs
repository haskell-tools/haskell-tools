{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, ConstraintKinds #-}

module SynTyFunInSuperClass where

import Definitions (CT)

type Syn a = CT a   {-* ConstraintKinds *-}

class Syn a => C a  {-* UndecidableInstances *-}
