module Life where

import Data.List (nub, union)

type Cell = (Int, Int) -- a cell on the board
type Board = [Cell]    -- a board is a list of living cells

-- E.g., the board [(1,0),(2,0),(2,1),(5,0)] represents
-- _XXXXX_
-- __XX___
-- _______
-- (the board extends infinitely down and to the left);
--
board :: Board
board = [(1,0),(2,0),(3,0),(4,0),(5,0),(2,1),(3,1)]

squareBoard :: Board
squareBoard = [(2,2),(2,3),(3,2),(3,3)]

glider :: Board
glider = [(3,0),(3,1),(3,2),(2,2),(1,1)]

neighbors :: Cell -> [Cell]
neighbors cell = 
  let deltas = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)] 
  in map (\(x, y) -> (fst cell + x, snd cell + y)) deltas

alive :: Board -> Cell -> Bool
alive board cell = cell `elem` board

livingNeighbors :: Board -> Cell -> [Cell]
livingNeighbors board cell =
  filter (alive board) (neighbors cell)

numLivingNeighbors :: Board -> Cell -> Int
numLivingNeighbors board cell = length (livingNeighbors board cell)

cellsWithLivingNeighbor :: Board -> Board
cellsWithLivingNeighbor board =
  nub $ foldl (\acc c -> neighbors c ++ acc) [] board

deadCellsWithLivingNeighbor :: Board -> Board
deadCellsWithLivingNeighbor living =
  let all = cellsWithLivingNeighbor board
  in filter (\c -> not $ c `elem` living) all

birthCells :: Board -> Board
birthCells board =
  let potentialKids = deadCellsWithLivingNeighbor board
  in foldl (\acc c -> if numLivingNeighbors board c == 3 then c:acc else acc) [] potentialKids

removeStarvedCells board = filter (\c -> numLivingNeighbors board c >= 2) board

removeOverpopulatedCells board = filter (\c -> numLivingNeighbors board c <= 3) board

next :: Board -> Board
next board = union (removeOverpopulatedCells $ removeStarvedCells board) (birthCells board)
  

-- play and display functions taken from
-- https://github.com/nbartlomiej/gameofhaskell/blob/master/GameOfHaskell.hs
main = play glider

play :: Board -> IO ()
play board = do
  display board
  getLine
  play $ next board

display :: Board -> IO ()
display board = do
  mapM_ (\x -> do
    mapM_ (\y -> putChar $ if (y,x) `elem` board then 'x' else ' ')[-10 .. 20]
    putChar '\n'
    )[-10 .. 20]

