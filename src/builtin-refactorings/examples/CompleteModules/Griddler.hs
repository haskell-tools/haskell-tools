module Griddler where
import Data.List

type Griddler = (Clues, Table)

type Cell     = Char
type Table    = [[Cell]]
type ClueLine = [Int]
type Clues    = ([ClueLine], [ClueLine])

unknown = 'u'
empty   = 'e'
full    = 'f'

duckClues  = (duckRows, duckCols)    :: Clues
poundClues = (poundRows,  poundCols) :: Clues

duckRows = [[3], [5], [4,3], [7], [5], [3], [5], [1,8],
            [3,3,3], [7,3,2], [5,4,2], [8,2], [10], [2,3], [6]]

duckCols = [[3], [4], [5], [4], [5], [6], [3,2,1], [2,2,5],
            [4,2,6], [8,2,3], [8,2,1,1], [2,6,2,1], [4,6], [2,4], [1]]


poundRows = [[4], [2,1], [1,2], [2,2], [2], [8], [2], [8],
             [2], [2], [2,2,2], [6,3], [2,5,3], [2,2,6], [4,4]]

poundCols = [[2],[4],[2,1],[1,1,2,1],[1,1,4],[11],[12],
             [2,1,1,2],[1,1,1,3],[1,1,1,2],[1,1,1,2],[3,3],[2,3],[3],[2]]

yinYangClues :: Clues
yinYangClues = (yinYangRows, yinYangCols)

yinYangRows = [[8],[4,4],[2,6],[1,3,2],[3,3],[8],[6],[2,5],
               [1,2,4],[2,5],[4,5],[8]]
yinYangCols = [[4,4],[3,3],[2,2],[2,2,2],[1,3,2,1],[1,4,2],
               [7,3],[3,7],[2,6],[10],[8],[4]]

flowerClues :: Clues
flowerClues = (flowerRows, flowerCols)

flowerRows = [[2,2],[1,1,1],[1,1,1],[1,5,2],[1,2,2,1],[1,5,1],
              [2,1,3],[2,2],[1],[2,1,2],[3],[1]]
flowerCols = [[2],[1,1],[2,1],[1,3,1,1],[1,3,1,1],[3,2,1],[1,3,5],
              [1,3,1,1],[2,1,1],[1,1,1],[1,1],[2]]

showCell :: Cell -> Char
showCell 'u' = '?'
showCell 'e' = ' '
showCell 'f' = '#'

showRow :: [Cell] -> String
showRow row = "|" ++ map showCell row ++ "|\n"

showTable :: Table -> String
showTable = concatMap showRow

empties :: Int -> [Cell]
empties n = replicate n empty

fulls :: Int -> [Cell]
fulls n = replicate n full

placeOneBlock :: Int -> Int -> [[Cell]]
placeOneBlock x n = [ empties k ++ fulls x ++ empties (n-k-x) | k <- [0..(n-x)] ]

emptyLineOptions :: ClueLine -> Int -> [[Cell]]
emptyLineOptions []  n                = [empties n]
emptyLineOptions [x] n                = placeOneBlock x n
emptyLineOptions (x:xs) n | m > n = []
                          | otherwise = [ empties k ++ fulls x ++ [empty] ++ rest | k <- [0..(n-m)], rest <- emptyLineOptions xs (n - (k + x + 1)) ]
  where
    m = sum (x:xs) + length xs

isMatching :: Cell -> Cell -> Bool
isMatching 'f' x | x /= full  = False
isMatching 'e' x | x /= empty = False
isMatching  _  _              = True

lineOptions :: ClueLine -> [Cell] -> [[Cell]]
--lineOptions clues row = map (map snd) $ filter (all pred) $ map (zip row) solutions
lineOptions clues row = filter (and . (zipWith isMatching row)) solutions
  where solutions = emptyLineOptions clues (length row)

combineOption :: Cell -> Cell -> Cell
combineOption 'f' 'f' = full
combineOption 'e' 'e' = empty
combineOption  _   _  = unknown

combineLineOptions :: [[Cell]] -> [Cell]
combineLineOptions = foldl1 (zipWith combineOption)

reduceLine :: ClueLine -> [Cell] -> [Cell]
reduceLine clues row = case lineOptions clues row of
                        []      -> row
                        options -> combineLineOptions options

reduceRows :: [ClueLine] -> Table -> Table
reduceRows = zipWith reduceLine

reduceTable :: Clues -> Table -> Table
reduceTable (rowClues, colClues) table
  = transpose $ reduceRows colClues (transpose rowReducedTable)
    where rowReducedTable = reduceRows rowClues table

emptyTable :: Int -> Int -> Table
emptyTable height width = replicate height (replicate width unknown)

recSolve :: Clues -> Table -> Table
recSolve (rowClues, colClues) table
  | newTable == table = table
  | otherwise         = recSolve (rowClues, colClues) newTable
  where newTable      = reduceTable (rowClues, colClues) table

solve :: Clues -> Table
solve (rows, cols) = recSolve (rows, cols) (emptyTable (length rows) (length cols))
