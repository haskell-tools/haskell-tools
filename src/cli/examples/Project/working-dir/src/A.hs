{-# LANGUAGE TemplateHaskell #-}
module A where

import Language.Haskell.TH

$(runIO (readFile "data.txt") >> return [])