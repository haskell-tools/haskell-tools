-----------------------------------------------------------------------------
-- |
-- Module      :  ReadFirst
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Read the first file that matches in a list of search paths.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.ReadFirst
  ( readFirst
  ) where

import System.IO        (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import Data.List      (intersperse)
import Control.Monad     (when)
import Language.Preprocessor.Cpphs.Position  (Posn,directory,cleanPath)

-- | Attempt to read the given file from any location within the search path.
--   The first location found is returned, together with the file content.
--   (The directory of the calling file is always searched first, then
--    the current directory, finally any specified search path.)
readFirst :: String             -- ^ filename
        -> Posn                 -- ^ inclusion point
        -> [String]             -- ^ search path
        -> Bool                 -- ^ report warnings?
        -> IO ( FilePath
              , String
              )                 -- ^ discovered filepath, and file contents

readFirst name demand path warn =
    case name of
       '/':nm -> try nm   [""]
       _      -> try name (cons dd (".":path))
  where
    dd = directory demand
    cons x xs = if null x then xs else x:xs
    try name [] = do
        when warn $
          hPutStrLn stderr ("Warning: Can't find file \""++name
                           ++"\" in directories\n\t"
                           ++concat (intersperse "\n\t" (cons dd (".":path)))
                           ++"\n  Asked for by: "++show demand)
        return ("missing file: "++name,"")
    try name (p:ps) = do
        let file = cleanPath p++'/':cleanPath name
        ok <- doesFileExist file
        if not ok then try name ps
          else do content <- readFile file
                  return (file,content)
