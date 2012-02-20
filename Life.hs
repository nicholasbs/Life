module Life where

import Data.List (nub, union)
import System.Posix (sleep, nanosleep)
import System.Console.ANSI (clearScreen)

type Cell = (Int, Int) -- a cell on the board
type Board = [Cell]    -- a board is a list of living cells

-- E.g., the board [(1,0),(2,0),(2,1),(5,0)] represents
-- _XXXXX_
-- __XX___
-- _______
-- (the board extends infinitely);

neighbors :: Cell -> [Cell]
neighbors cell =
  let deltas = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]
  in map (\(x, y) -> (fst cell + x, snd cell + y)) deltas

alive :: Board -> Cell -> Bool
alive board cell = cell `elem` board

livingNeighbors :: Board -> Cell -> [Cell]
livingNeighbors board cell = filter (alive board) (neighbors cell)

numLivingNeighbors :: Board -> Cell -> Int
numLivingNeighbors board cell = length (livingNeighbors board cell)

cellsWithLivingNeighbor :: Board -> Board
cellsWithLivingNeighbor board =
  nub $ foldl (\acc c -> neighbors c ++ acc) [] board

deadCellsWithLivingNeighbor :: Board -> Board
deadCellsWithLivingNeighbor living =
  let all = cellsWithLivingNeighbor living
  in filter (\c -> not $ c `elem` living) all

-- Rule: Dead cells with exactly 3 living neighbors become live cells
birthCells :: Board -> Board
birthCells board =
  let potentialKids = deadCellsWithLivingNeighbor board
  in foldl (\acc c -> if numLivingNeighbors board c == 3 then c : acc else acc) [] potentialKids

-- Rule: Living cells with fewer than 2 living neighbors die
removeStarvedCells board = filter (\c -> numLivingNeighbors board c >= 2) board

-- Rule: Living cells with more than 3 living neighbors die
removeOverpopulatedCells board = filter (\c -> numLivingNeighbors board c <= 3) board

next :: Board -> Board
next board = union (removeOverpopulatedCells $ removeStarvedCells board) (birthCells board)


-- Test boards
board :: Board
board = [(1,0),(2,0),(3,0),(4,0),(5,0),(2,1),(3,1),(10,2),(11,2),(11,0),(-2,-4),(-2,-5),(-2,-6)]

glider :: Board
glider = [(3,0),(3,1),(3,2),(2,2),(1,1)]


-- play and display function inspired by
-- https://github.com/nbartlomiej/gameofhaskell/blob/master/GameOfHaskell.hs

terminalCols = 79
terminalRows = 24

terminalLeft = 0 - terminalCols `div` 2
terminalRight = terminalCols `div` 2
terminalTop = 0 - terminalRows `div` 2
terminalBottom = terminalRows `div` 2

main = do
  clearScreen
  play board

play :: Board -> IO ()
play board = do
  display board
  getLine -- comment out and uncomment below for "animation"
  -- nanosleep 200000000 -- 200 ms
  play $ next board

display :: Board -> IO ()
display board = do
  mapM_ (\x -> do
    mapM_ (\y -> putChar $ if alive board (y,x) then 'X' else ' ') [terminalLeft .. terminalRight]
    putChar '\n'
    ) [terminalTop .. terminalBottom]
