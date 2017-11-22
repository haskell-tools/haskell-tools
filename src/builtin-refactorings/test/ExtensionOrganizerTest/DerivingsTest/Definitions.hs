module Definitions
  ( module Definitions
  , module X
  ) where

import Data.Data                  as X (Data)
import Data.Typeable              as X (Typeable)
import Data.Ix                    as X (Ix)
import GHC.Generics               as X (Generic)
import Language.Haskell.TH.Syntax as X (Lift)

class C1 a where
  f1 :: a -> ()
  f1 _ = ()
