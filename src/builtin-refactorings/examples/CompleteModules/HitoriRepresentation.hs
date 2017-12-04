module HitoriRepresentation where

import Data.List

data CellState = Shaded | Circled | Undecided
  deriving (Eq, Show)
type Position = (Int, Int)
type Cell     = ((Position, Int), CellState)
type Row      = [Cell]
type Column   = [Cell]
type Table    = ([Row], [Column])

prettyEasy1 :: Table
prettyEasy1 = toTable prettyEasy1Numbers

easy1 :: Table
easy1 = toTable easy1Numbers

medium1 :: Table
medium1 = toTable medium1Numbers

hard1 :: Table
hard1 = toTable hard1Numbers

veryHard1 :: Table
veryHard1 = toTable veryHard1Numbers

veryHard2 :: Table
veryHard2 = toTable veryHard2Numbers

superHard1 :: Table
superHard1 = toTable superHard1Numbers

prettyEasy1Numbers :: [Int]
prettyEasy1Numbers = [ 2, 4, 3, 5, 1
                     , 1, 2, 5, 2, 4
                     , 5, 1, 2, 4, 1
                     , 1, 5, 4, 2, 2
                     , 3, 2, 1, 2, 2
                     ]

easy1Numbers :: [Int]
easy1Numbers = [ 4, 2, 3, 3, 1
               , 3, 1, 2, 4, 5
               , 4, 4, 3, 5, 3
               , 2, 3, 5, 1, 1
               , 4, 4, 1, 2, 4
               ]

medium1Numbers :: [Int]
medium1Numbers = [ 2, 3, 2, 1, 1
                 , 5, 4, 5, 3, 1
                 , 1, 2, 5, 3, 3
                 , 4, 5, 3, 5, 2
                 , 4, 5, 1, 2, 2
                 ]

hard1Numbers :: [Int]
hard1Numbers = [ 5, 2, 1, 4, 1
               , 2, 5, 2, 1, 4
               , 2, 2, 3, 5, 2
               , 4, 1, 1, 3, 2
               , 1, 1, 4, 2, 1
               ]

veryHard1Numbers :: [Int]
veryHard1Numbers = [ 5, 3, 1, 4, 1
                   , 3, 2, 3, 1, 4
                   , 1, 3, 4, 3, 2
                   , 3, 1, 1, 5, 3
                   , 2, 4, 5, 3, 4
                   ]

veryHard2Numbers :: [Int]
veryHard2Numbers = [ 4, 5, 1, 2, 4
                   , 5, 2, 4, 5, 2
                   , 1, 2, 3, 4, 1
                   , 4, 1, 2, 1, 5
                   , 2, 4, 3, 5, 2
                   ]

superHard1Numbers :: [Int]
superHard1Numbers = [ 3, 8, 6, 8, 1, 2, 7, 5
                    , 4, 7, 2, 6, 2, 1, 5, 4
                    , 8, 2, 4, 4, 6, 3, 8, 7
                    , 2, 3, 7, 1, 4, 2, 1, 3
                    , 5, 7, 4, 3, 7, 8, 4, 6
                    , 8, 1, 8, 2, 3, 7, 6, 2
                    , 6, 1, 3, 2, 5, 4, 2, 1
                    , 8, 6, 4, 1, 5, 7, 4, 2
                    ]

gridSize :: Int
gridSize = 5

mapPositions :: [a] -> [(Position, a)]
mapPositions = zipWith f [0..]
  where f ind x = ((ind `div` gridSize, ind `mod` gridSize), x)

partitions :: Int -> [a] -> [[a]]
partitions 0 _  = []
partitions _ [] = []
partitions n xs
  | (ys, rest) <- splitAt n xs = ys : partitions n rest

toRows :: [a] -> [[a]]
toRows = partitions gridSize

toCols :: [a] -> [[a]]
toCols = transpose . toRows

toTable :: [Int] -> Table
toTable xs = (toRows cells, toCols cells)
  where cells = zip (mapPositions xs) (repeat Undecided)



value :: Cell -> Int
value = snd . fst

position :: Cell -> Position
position = fst . fst

state :: Cell -> CellState
state = snd

isShaded :: Cell -> Bool
isShaded = (== Shaded) . state

isCircled :: Cell -> Bool
isCircled = (== Circled) . state

isUndecided :: Cell -> Bool
isUndecided = (== Undecided) . state

notShaded :: Cell -> Bool
notShaded = not . isShaded

shadeCell :: Cell -> Cell
shadeCell (((x,y),v),_) = (((x,y),v),Shaded)

circleCell :: Cell -> Cell
circleCell (((x,y),v),_) = (((x,y),v),Circled)



isValidPos :: Position -> Bool
isValidPos (x,y) = x >= 0 && y >= 0 && x < gridSize && y < gridSize

cellAt :: Position -> Table -> Cell
cellAt (x,y) t = fst t !! x !! y

update :: Int -> (a -> a) -> [a] -> [a]
update i f xs
  | i < 0                     = xs
  | (ys,z:zs) <- splitAt i xs = ys ++ [f z] ++ zs
  | otherwise                 = xs


updateCell :: (Cell -> Cell) -> Position -> Table -> Table
updateCell f (x,y) t = (rows', cols')
  where rows' = update x (update y f) (fst t)
        cols' = update y (update x f) (snd t)

updateCells :: (Cell -> Cell) -> [Position] -> Table -> Table
updateCells f ps t = foldl (flip $ updateCell f) t ps

shadeAll :: [Position] -> Table -> Table
shadeAll = updateCells shadeCell

circleAll :: [Position] -> Table -> Table
circleAll = updateCells circleCell

circle :: Position -> Table -> Table
circle p = circleAll [p]

shade :: Position -> Table -> Table
shade p = shadeAll [p]

nthRow :: Int -> Table -> Row
nthRow n = (!! n) . fst

nthCol :: Int -> Table -> Column
nthCol n = (!! n) . snd

withoutNth :: [a] -> Int -> [a]
withoutNth xs i
  | i < 0                     = xs
  | (ys,z:zs) <- splitAt i xs = ys ++ zs
  | otherwise                 = take i xs

rowWithout :: Position -> Table -> Row
rowWithout (x,y) t = nthRow x t `withoutNth` y

colWithout :: Position -> Table -> Column
colWithout (x,y) t = nthCol y t `withoutNth` x


filterTable :: (Cell -> Bool) -> Table -> [Cell]
filterTable pred = filter pred . concat . fst

shadedPos :: Table -> [Position]
shadedPos =  map position . filterTable isShaded

notShadedPos :: Table -> [Position]
notShadedPos = map position . filterTable notShaded

circledPos :: Table -> [Position]
circledPos = map position . filterTable isCircled

undecidedPos :: Table -> [Position]
undecidedPos = map position . filterTable isUndecided
