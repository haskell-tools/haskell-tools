{-# LANGUAGE PackageImports #-}
module Module.Import where

import Data.List
import "base" Data.List
import {-# SOURCE #-} Data.List
import qualified Data.List
import Data.List as List
import Data.List(map,(++))
import Data.Function hiding ((&))
