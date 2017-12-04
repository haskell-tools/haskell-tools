module Hitori
  ( module Hitori
  , module X
  ) where

import HitoriRepresentation as X
import Data.Function        as X
import Data.List            as X

import Debug.Trace (traceShow, trace)

{-# ANN module "HLint: ignore Use section" #-}

debug x = traceShow x x

debugT t = traceTable t t

debugCT ct = traceTable (snd ct) ct

traceTable t = trace (tmask ++ "\n" ++ "-------------")
  where tmask = showTableMask t

traceTable' m t = trace (m ++ "\n" ++ tmask ++ "\n" ++ "-------------")
  where tmask = showTableMask t

showTableMask :: Table -> String
showTableMask (rows,_) = intercalate "\n" . map (concatMap (showState . state)) $ rows
  where showState Shaded    = "#"
        showState Circled   = "O"
        showState Undecided = "."

showTableMaskIO = putStrLn . showTableMask


{- HITORI RULES

  A table is a solution iff there are no undecided cells,
  and the table obeys the following rules:

  1st rule: Every circled cell is unique in its row and column.
  2nd rule: The table must not be partioned,
            i.e: from any circled cell there exists a route to any other circled cell.
            One can step from one cell to another iff they are adjacent.
  3rd rule: There must not be any adjacent shaded cells.

-}



matchingCells :: Position -> Table -> [Cell]
matchingCells p t = ys ++ xs
  where xs = filter (matchesWith c) . rowWithout p $ t
        ys = filter (matchesWith c) . colWithout p $ t
        matchesWith x y = value x == value y
        c = cellAt p t

undecidedMatches :: Position -> Table -> [Cell]
undecidedMatches p = filter isUndecided . matchingCells p

notShadedMatches :: Position -> Table -> [Cell]
notShadedMatches p = filter notShaded . matchingCells p

isUnique :: Position -> Table -> Bool
isUnique p = null . notShadedMatches p

neighbourPositions :: Position -> [Position]
neighbourPositions (x,y) = filter isValidPos allPos
  where allPos = [ (x-1, y)
                 , (x+1, y)
                 , (x  , y-1)
                 , (x  , y+1)
                 ]

neighbours :: Position -> Table -> [Cell]
neighbours p t = map (`cellAt` t) . neighbourPositions $ p

isSolution :: Table -> Bool
isSolution t = noundecidedPos
            && isEveryCellUnique t
            && noAdjacentShadedCells
            && not (isPartitioned t)
  where noundecidedPos        = null . undecidedPos    $ t
        noAdjacentShadedCells = not  . anyAdjacentShaded $ t

anyAdjacentShaded :: Table -> Bool
anyAdjacentShaded t = any isShaded . concatMap (`neighbours` t) . shadedPos $ t

-- every circled cell is unique
isEveryCellUnique :: Table -> Bool
isEveryCellUnique t = all (`isUnique` t) . circledPos $ t

isPartitioned :: Table -> Bool
isPartitioned t
  | ps@(x:_) <- notShadedPos t = sort ps /= reachables x
  | otherwise                  = False
    where reachables = sort . map position . flip reachablesFrom t . flip cellAt t

-- calculates all reachable unshaded cells from an unshaded cell
reachablesFrom :: Cell -> Table -> [Cell]
reachablesFrom c t
  | notShaded c = map (`cellAt` t) . snd . reachablesFrom' pos $ t
  | otherwise   = []
    where pos   = position c

reachablesFrom' :: Position -> Table -> (Table, [Position])
reachablesFrom' p t = foldl f (t',[p]) notShadedNeighbours
  where t' = shade p t
        notShadedNeighbours = map position . filter notShaded . neighbours p $ t
        f (t,rs) cur
          | notShaded (cellAt cur t) = (t', xs ++ rs)
          | otherwise                = (t,        rs)
          where (t',xs) = reachablesFrom' cur t

undecidedNeighbours :: Position -> Table -> [Cell]
undecidedNeighbours p t = filter isUndecided . neighbours p $ t


undecidedUniques :: Table -> [Position]
undecidedUniques t = filter (`isUnique` t) . undecidedPos $ t

circleUniques :: Table -> Table
circleUniques t = circleAll ps t
  where ps = undecidedUniques t

impact :: Position -> Table -> Int
impact p t = length . filter (/= cellAt p t) . nub $ (ns ++ ms ++ ms')
  where ms  = undecidedMatches p t
        ms' = concatMap (flip undecidedNeighbours t . position) ms
        ns  = undecidedNeighbours p t
        ns' = concatMap (flip undecidedNeighbours t . position) ns

-- only for testing (should be 0 on a solved table)
impactSum :: Table -> Int
impactSum t = sum . map (`impact` t) $ ps
  where ps = [(x,y) | x <- [0..4], y <- [0..4]]

orderByImpact :: Table -> [Position]
orderByImpact t = sortBy (flip compare `on` impact' t) . undecidedPos $ t
  where impact' = flip impact

highestImpact :: Table -> Position
highestImpact t = maximumBy (compare `on` impact' t) . undecidedPos $ t
  where impact' = flip impact


type Checked a = (Bool, a)

isValid :: Checked a -> Bool
isValid = fst


{-
  A version of foldl that can stop the execution.

  We could define a function f for foldl such that the end results of the
  two folds are the same, but that wouldn't stop the traversal.

  This version will recognize that the accumulated value is invalid,
  and shortcut the folding.
  Also note that the type of the stepping function is much cleaner this way.
-}
breakableFold :: (b -> a -> Checked b) -> b -> [a] -> Checked b
breakableFold f acc = breakableFold' f (True, acc)

breakableFold' :: (b -> a -> Checked b) -> Checked b -> [a] -> Checked b
breakableFold' f acc xs
  | (True, acc') <- acc
  , (x:xs)       <- xs  = breakableFold' f (f acc' x) xs
  | otherwise           = acc


  {-
    Recursively shading means that:
      - we shade the given cell
      - then check whether the table is still valid
      - if it is, then we continue circling/shading other cells, by applying the rules

    1st case: After shading the cell, checks whether the table is valid.
              If it is invalid, then we return False.
              (see Rule #2, #3)

    2nd case: If we can't continue, we will return.

    3rd case: We check for the shaded cells undecided neighbours,
              and circle all of them recursively (see Rule #3).

              Also we check whether there are any unique cells
              in the same row or column, and if there are, we circle them recursively.
              (We can do so if a cell is unique at ANY POINT
                - because why would we shade it?)
  -}
recShade :: Position -> Table -> Checked Table
recShade p t
  | isPartitioned t' || anyAdjacentShaded t' = (False, t)
  | null nps && null mps                     = (True, t')
  | otherwise                                = breakableFold' (flip recCircle) t'' mps
  where t'  = shade p t
        t'' = breakableFold (flip recCircle) t' nps
        nps = map position . undecidedNeighbours p $ t' -- could be t as well
        mps = filter (`isUnique` snd t'') . map position . undecidedMatches p . snd $ t''

{-
  Recursively circling means that:
    - we circle the given cell
    - then check whether the table is still valid
    - if it is, then we continue circleing/shading other cells, by applying the rules

  1st case: Checks whether there are any circled matching cells (NOT THE SAME as isUnique - see NOTE)
            Also if we wanted to circle such a cell,
            then the table wasn't valid in the first place, hence we return False.
            (see Rule #1)

  2nd case: We circle the given cell, and if there aren't any
            undecided matching cells, then we return.

  3rd case: If there are such cells, then we recursively shade them.
            (see Rule #1)

  NOTE: isUnique checks whether there are any NOT SHADED matching cells.
        When we are in the function, we can presume that the circling
        was a consequence of a rule application (e.g.: after shading a cell)
        In this case we can circle it blindly, and check only for the above condition.

-}
recCircle :: Position -> Table -> Checked Table
recCircle p t
  | notUniqueCircled = (False, t)
  | null ps          = (True, t')
  | otherwise        = breakableFold (flip recShade) t' ps
  where t' = circle p t
        ps = map position . undecidedMatches p $ t' -- could be t as well
        notUniqueCircled = any isCircled . matchingCells p $ t'



mergeStates :: CellState -> CellState -> CellState
mergeStates s1 s2
  | s1 == s2  = s1
  | otherwise = Undecided

mergeCells :: Cell -> Cell -> Cell
mergeCells (x1,s1) (_,s2) = (x1, mergeStates s1 s2)

mergeTables :: Table -> Table -> Table
mergeTables (rows1,_) (rows2,_) = (rows', transpose rows')
  where rows' = zipWith (zipWith mergeCells) rows1 rows2

mergeCheckedTables :: Checked Table -> Checked Table -> Checked Table
mergeCheckedTables (b1, t1) (b2, t2)
  | b1 && b2  = (True, mergeTables t1 t2)
  | b1        = (True, t1)
  | b2        = (True, t2)
  | otherwise = (False, error "Should not have accessed this")

guess :: Position -> Table -> (Checked Table, Checked Table)
guess p t = (recShade p t, recCircle p t)

{-
 If only one operation is valid,
 then we will perform a reduction step on the table.

 No reduction may only be performed when both operations are valid,
 since in that case the merged table could contain no new inormation.

 In the latter situation we guess recursively.
 In this process we calculate some possible valid states of that table,
 and determine their common cells. (this may still yield no extra information)
-}
recGuess :: Position -> Table -> Checked Table
recGuess p t
  | validShade && validCircle = mergeCheckedTables (recGuess hi1 t1) (recGuess hi2 t2)
  | otherwise                 = mergeCheckedTables ct1 ct2
  where ct1@(validShade,  t1) = fst possibilities
        ct2@(validCircle, t2) = snd possibilities
        possibilities         = guess p t

        hi1 = highestImpact t1
        hi2 = highestImpact t2

solve :: Table -> Table
solve t = heuristicSolve . circleUniques $ t

{-
  The validity check is only important if we get an unsolvable puzzle.
  Also, this process may not find a solution even if it exist.
  (The recGuess function is heuristic, so it may not yield extra information
  even though the solve' function tries every single undecided cell.)
  If we make the recGuess function complete, we will always find a solution.
-}
heuristicSolve :: Table -> Table
heuristicSolve t
  | isSolution t     = t
  | (_, t'):_ <- cts = heuristicSolve t'
  | otherwise        = error "Has no solution"
  where ps  = orderByImpact t
        cts = filter ((/= t) . snd) . filter isValid . map (`recGuess` t) $ ps



-- COMPLETE SOLUTION

guess'' :: Position -> Table -> [Table]
guess'' p t = map snd . filter isValid $ [fst possibilities, snd possibilities]
  where possibilities = guess p t

-- step will always get a valid table
step :: Table -> [Table]
step t
  | [] <- ps  = [t]
  | otherwise = concatMap (`guess''` t) ps
  where ps = orderByImpact t

iterateStep :: [Table] -> [Table]
iterateStep ts
  | s:_ <- filter noundecidedPos ts = [s]
  | otherwise                         = iterateStep . concatMap step $ ts
  where noundecidedPos = null . undecidedPos

completeSolve :: Table -> Table
completeSolve t
  | t':_ <- iterateStep [t] = t'
  | otherwise = error "Has no solution"
