module Generator
  ( Difficulty(..)
  , generatePuzzle
  ) where

import Control.Arrow (first, second, (>>>), (***), app)
import Data.Tuple (swap)
import System.Random (StdGen)

import Definition
import Solver
import Util (replace2DList)
import Shuffle (shuffleList)

data Difficulty = Easy | Normal | Hard

emptyPuzzle :: Int -> Puzzle
emptyPuzzle size = Puzzle size []

randomSolution :: StdGen -> Int -> Solution
randomSolution gen size =
  let empty = emptyPuzzle size
      sols  = solve empty (randSolver gen)
  in  head sols

generatePuzzle :: StdGen -> Int -> Difficulty -> Puzzle
generatePuzzle gen size diff =
  let dimensions            = square size
      cellNums              = [0 .. square dimensions - 1]
      (cellsToRemove, gen') = first (map $ getCellFromNum dimensions) $
                                    getCellsToRemove gen diff cellNums
      solution              = randomSolution gen' size
      board                 = removeCells cellsToRemove solution
  in  ensureUniqueSolution cellsToRemove solution $ Puzzle size board

getCellsToRemove :: StdGen -> Difficulty -> [Int] -> ([Int], StdGen)
getCellsToRemove gen diff xs =
  let (shuffled, gen') = shuffleList xs gen
      len              = length xs `div` case diff of
                           Easy   -> 2
                           Normal -> 3
                           Hard   -> 4

  in  (drop len shuffled, gen')

removeCells :: [Cell] -> Solution -> Board
removeCells cells sol = removeByCell cells $ map (map Just) sol
  where
    removeByCell :: [Cell] -> Board -> Board
    removeByCell []                 board = board
    removeByCell ((row, col):cells) board = removeByCell cells $ replaceCell row col board

    replaceCell :: Int -> Int -> Board -> Board
    replaceCell row col = replace2DList row col Nothing

ensureUniqueSolution :: [Cell] -> Solution -> Puzzle -> Puzzle
ensureUniqueSolution cells sol puz
    | length cells < step = puz
    | otherwise           =
        let sols                          = take 2 $ solve puz defaultSolver
            (cellsToRestore, cellsRemain) = splitAt step cells
        in  if length sols <= 1
              then puz
              else ensureUniqueSolution cellsRemain sol $ restoreCells cellsToRestore sol puz
  where
    step :: Int
    step = gameSize puz

restoreCells :: [Cell] -> Solution -> Puzzle -> Puzzle
restoreCells []           _   puz = puz
restoreCells (cell:cells) sol puz =
  restoreCells cells sol $ replaceCellInPuzzle cell (Just $ getSolValue cell) puz

  where
    getSolValue :: Cell -> Value
    getSolValue = ((sol !!) *** flip (!!)) >>> swap >>> app

replaceCellInPuzzle :: Cell -> Maybe Value -> Puzzle -> Puzzle
replaceCellInPuzzle (row, col) value (Puzzle size board) =
  Puzzle size $ replace2DList row col value board
