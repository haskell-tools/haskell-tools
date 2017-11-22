{-# LANGUAGE RecordWildCards #-}

module InPattern where

import Definitions

f :: T -> Bool
f T{a=1,c=1,..} = True {-* RecordWildCards *-}
f T{..} = True         {-* RecordWildCards *-}
