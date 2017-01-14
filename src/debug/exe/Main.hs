module Main where

import Language.Haskell.Tools.Debug
import System.Environment

main :: IO ()
main = do args <- getArgs
          if length args < 3 then do progName <- getProgName
                                     putStrLn (progName ++ " <command/-> <workingdir> <modulename> [arguments]")
                             else demoRefactor (dropWhile (=='-') (args !! 0)) (args !! 1) (drop 3 args) (args !! 2)