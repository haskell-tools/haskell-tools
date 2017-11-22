module ExtensionOrganizerTest.AnnotationParser where

import Data.List
import qualified Data.Map.Strict as SMap (Map, empty, insertWith)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad (Extension, LogicalRelation(..)) -- for Ord Extension

{-# ANN module "HLint: ignore Use zipWith" #-}


getExtensionAnnotations :: String -> SMap.Map (LogicalRelation Extension) [Int]
getExtensionAnnotations s = foldl f SMap.empty (parseFile s)
  where f m (num, exts) = foldl (g num) m exts
        g num m' ext = SMap.insertWith (++) (LVar ext) [num] m'

parseFile :: String -> [(Int, [Extension])] -- SMap.Map Extension [Int]
parseFile = map parseLine . zip [1..] . lines

parseLine :: (Int, String) -> (Int, [Extension])
parseLine (num, line) = (num, parseAnnot line)

parseAnnot :: String -> [Extension]
parseAnnot = map read . delimit (== ',') . getAnnot

getAnnot :: String -> String
getAnnot = concat . takeWhile (not . isPrefixOf "*-}") . tail' . dropWhile (not . isPrefixOf "{-*" ) . words
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
