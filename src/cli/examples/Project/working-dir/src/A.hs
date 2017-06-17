{-# LANGUAGE TemplateHaskell #-}
module A where

import Language.Haskell.TH
import System.FilePath

$(location >>= \loc -> runIO (readFile (takeDirectory (takeDirectory (loc_filename loc)) </> "data.txt")) >> return [])
