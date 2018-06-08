{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module BadTyVars where

import Definitions


-- NOTE: This instance need FlexibleContexts, but if UndecidableInstances
-- is turned on, it compiles. Watch out for this!
instance C (a,a) => C a  {-* UndecidableInstances, FlexibleInstances *-}
