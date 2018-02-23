{-# LANGUAGE OverloadedStrings #-}

module Other where

import GHC.Exts (IsString(..))
import Data.Text

f :: Text -> Text
f "asd" = "qwe"     {-* OverloadedStrings, OverloadedStrings *-}
f x     = x
