module ExtensionOrganizerTest.AnnotationParser
  (getLocalExtensionAnnotations, getGlobalExtensionAnnotations)
  where

import Data.List
import qualified Data.Map.Strict as SMap (Map, empty, insertWith)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap (Extension, LogicalRelation(..))

{-# ANN module "HLint: ignore Use zipWith" #-}


getGlobalExtensionAnnotations :: String -> [Extension]
getGlobalExtensionAnnotations s
  | [] <- getGlobalAnnot s = []
  | otherwise              = parseGlobalAnnot . getGlobalAnnot $ s

getLocalExtensionAnnotations :: String -> SMap.Map (LogicalRelation Extension) [Int]
getLocalExtensionAnnotations s = foldl f SMap.empty (parseFile s)
  where f m (num, exts) = foldl (g num) m exts
        g num m' ext = SMap.insertWith (++) ext [num] m'

parseFile :: String -> [(Int, [LogicalRelation Extension])] -- SMap.Map Extension [Int]
parseFile = map parseLine . zip [1..] . lines

parseLine :: (Int, String) -> (Int, [LogicalRelation Extension])
parseLine (num, line) = (num, parseLocalAnnot . getLocalAnnot $ line)

parseLocalAnnot :: String -> [LogicalRelation Extension]
parseLocalAnnot = map parseRelation . delimit (== ',')

-- NOTE: Currently only parses OR relation
parseRelation :: String -> LogicalRelation Extension
parseRelation = foldl1 (:||:) . map (LVar . read) . delimit (== '+')

parseGlobalAnnot :: String -> [Extension]
parseGlobalAnnot = map read . delimit (== ',')

getGlobalAnnot :: String -> String
getGlobalAnnot = getAnnot "{-@" "@-}"

getLocalAnnot :: String -> String
getLocalAnnot = getAnnot "{-*" "*-}"

getAnnot :: String -> String -> String -> String
getAnnot start end = concat . takeWhile (not . isPrefixOf end) . tail' . dropWhile (not . isPrefixOf start ) . words
  where tail' [] = []
        tail' xs = tail xs

delimit :: (a -> Bool) -> [a] -> [[a]]
delimit pred xs = delimit' pred xs []

delimit' :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
delimit' _ [] acc = acc
delimit' pred xs acc = delimit' pred l2 (acc ++ [l1])
  where (l1, l2) = break' pred xs

-- exludes the elemnt for which the predicate is true
break' _ xs@[] = (xs, xs)
break' p (x:xs')
  | p x        = ([],xs')
  | otherwise  = let (ys,zs) = break' p xs' in (x:ys,zs)
