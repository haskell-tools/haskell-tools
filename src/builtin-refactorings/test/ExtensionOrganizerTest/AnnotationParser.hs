module ExtensionOrganizerTest.AnnotationParser
  -- (getLocalExtensionAnnotations, getGlobalExtensionAnnotations)
  where

import ExtensionOrganizerTest.Parser

import Data.List
import qualified Data.Map.Strict as SMap (Map, empty, insertWith)
import Language.Haskell.Tools.Refactor.Utils.Extensions (canonExt)
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMap


{-# ANN module "HLint: ignore Use zipWith" #-}


getGlobalExtensionAnnotations :: String -> [Extension]
getGlobalExtensionAnnotations s
  | [] <- getGlobalAnnot s = []
  | otherwise              = parseGlobalAnnot . getGlobalAnnot $ s

getLocalExtensionAnnotations :: String -> SMap.Map (LogicalRelation Extension) [Occurence Int]
getLocalExtensionAnnotations s = foldl f SMap.empty (parseFile s)
  where f m (num, exts) = foldl (g num) m exts
        g num m' ext = SMap.insertWith (++) ext [num] m'

parseFile :: String -> [(Occurence Int, [LogicalRelation Extension])] -- SMap.Map Extension [Int]
parseFile = concatMap (separateOccurences . parseLine) . zip [1..] . lines

-- | Separates the occurences into hints and evidence.
-- The resulting list will always contain two elements
separateOccurences :: (Int, [Occurence (LogicalRelation Extension)]) ->
                      [(Occurence Int, [LogicalRelation Extension])]
separateOccurences (ln, occs) = [(Evidence ln, evs'), (Hint ln, hints')]
  where (evs, hints) = partition isEvidence occs
        evs' = map fromOcc evs
        hints' = map fromOcc hints

        isEvidence (Evidence _) = True
        isEvidence _            = False

        fromOcc (Evidence x) = x
        fromOcc (Hint x)     = x

parseLine :: (Int, String) -> (Int, [Occurence (LogicalRelation Extension)])
parseLine (num, line) = (num, parseLocalAnnot . getLocalAnnot $ line)

parseLocalAnnot :: String -> [Occurence (LogicalRelation Extension)]
parseLocalAnnot = map parseRelation . prepareAnnot

parseRelation :: String -> Occurence (LogicalRelation Extension)
parseRelation = execParser relation

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

-- exludes the element for which the predicate is true
break' _ xs@[] = (xs, xs)
break' p (x:xs')
  | p x        = ([],xs')
  | otherwise  = let (ys,zs) = break' p xs' in (x:ys,zs)

---------

prepareAnnot :: String -> [String]
prepareAnnot = map (filter (not . isSpace)) .  delimit (== ',')

crop :: String -> String
crop = dropWhile isSpace . reverse . dropWhile isSpace . reverse

relation :: Parser (Occurence (LogicalRelation Extension))
relation = Hint     <$> (token "(" *> relation' <* token ")")
       <|> Evidence <$> relation'

-- | A relation parser that tries to reduce the left-hand side of operators,
-- before applíing the left-recursive rules.
-- Also the rules are a bit convoluted in order to respect the precedence
-- of the operators
relation' :: Parser (LogicalRelation Extension)
relation' = primRel
       <|> (:&&:) <$> primRel  <*> (token "*" *> primRel)
       <|> (:||:) <$> primRel  <*> (token "+" *> relation')
       <|> (:||:) <$> relation' <*> (token "+" *> relation')
       <|> (:&&:) <$> relation' <*> (token "*" *> relation')
       <|> Not    <$> (token "~" *> relation')

  where primRel = (lVar . readExt) <$> word
              <|> Not <$> (token "~" *> primRel)

        readExt :: String -> Extension
        readExt = read . canonExt
