{-# LANGUAGE FlexibleContexts #-}

module Solver
  ( solve
  , defaultSolver
  , randSolver
  ) where

{-
 - My own adaptation of a Sudoku solver using StateT and [] for backtracking
 -
 - Inspired from: https://wiki.haskell.org/Sudoku#Backtrack_monad_solver
 -}

import Control.Arrow
import Data.List (delete)
import Data.Maybe (maybeToList)

import Control.Monad.Plus (mfromMaybe)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans

import System.Random
import Shuffle (shuffleList)


import Definition
import Util (replace2DList, set2DAList)





------- Contraints and Setting Cell Values -------

setCellValue :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
             => Cell -> Value -> m ()
cell@(row, col) `setCellValue` value = do

    {-
     - Get previously remaining values and guard that the value to set is in
     - the previously remaining values
     -}
    remainingVals <- gets $ getCellValues cell
    guard $ value `elem` remainingVals

    env <- ask
    let size       = envSize env
        dimensions = envDimensions env
        blocks     = envBlocks env
        values     = envValues env

        blk        = blockNum size cell
        currentBlk = blocks !! blk
        cellsRange = [0 .. dimensions - 1]

    -- Set the value in cell, row, col and blk options
    modifyCellOpts $ replaceCellOpts row col   [value]
    modifyRowOpts  $ replaceOpts     row value [cell]
    modifyColOpts  $ replaceOpts     col value [cell]
    modifyBlkOpts  $ replaceOpts     blk value [cell]

    -- Remove all other values from all occurrences of this 'cell'
    sequence_ [constrainCell v     cell     | v <- values,     v /= value]

    -- Remove current 'value' from current 'row' with col /= this 'col'
    sequence_ [constrainCell value (row, c) | c <- cellsRange,  c /= col ]

    -- Remove current 'value' from current 'col' with row /= this 'row'
    sequence_ [constrainCell value (r, col) | r <- cellsRange,  r /= row ]

    {-
     - Remove current 'value' from cells in currentBlk where
     - the block cell /= this 'cell'
     -}
    sequence_ [constrainCell value blkCell  | blkCell <- currentBlk
                                            , blkCell /= cell ]

-- Gets the list of possible values of a given Cell from CellOptions
getCellValues :: Cell -> Options -> [Value]
getCellValues (row, col) = cellOpts >>> (!! row) >>> (!! col)

{-
 - The 'Alternative' and 'MonadPlus' implementations of []:
 -
 - instance Alternative [] where
 -   empty = []
 -   (<|>) = (++)
 -
 - instance MonadPlus [] where
 -   mzero = empty
 -   mplus = (<|>)
 -}

{-
 - Assert that the cell to constrain must have the 'val' as one of its
 - possible values.
 -
 - Otherwise, solution is invalid and thus 'mzero'.
 -}
constrainCell :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
              => Value
              -> Cell
              -> m ()
constrainCell val cell@(row, col) = do
    env <- ask
    let size = envSize env
        blk  = blockNum size cell

    constrainCellOpts cell val
    constrainOpts     row  val cell modifyRowOpts rowOpts
    constrainOpts     col  val cell modifyColOpts colOpts
    constrainOpts     blk  val cell modifyBlkOpts blkOpts

  where
    constrainCellOpts :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
                      => Cell -> Value -> m ()
    constrainCellOpts cell@(row, col) valToRemove = do
      valsRemain <- gets $ getCellValues cell
      case valsRemain of
        [val] -> guard (val /= valToRemove)
        [_,_] -> when (valToRemove `elem` valsRemain) $
                      cell `setCellValue` head (delete valToRemove valsRemain)
        (_:_) -> modifyCellOpts $ replaceCellOpts row col
                                  (delete valToRemove valsRemain)
        _     -> mzero

    constrainOpts :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
                  => Int -> Value -> Cell
                  -> ((ValueOptions -> ValueOptions) -> m ())
                  -> (Options -> ValueOptions)
                  -> m ()
    constrainOpts index value cellToRemove modifyOpts getOpts = do
      cellsRemain <- gets $ getOpts >>> (!! index) >>> lookup value
                                    >>> mfromMaybe >>> join
      case cellsRemain of
        [cell] -> guard (cell /= cellToRemove)
        [_,_]  -> when (cellToRemove `elem` cellsRemain) $
                       head (delete cellToRemove cellsRemain) `setCellValue` value
        (_:_)  -> modifyOpts $ replaceOpts index value
                               (delete cellToRemove cellsRemain)
        _      -> mzero


------- Solutions -------

solutions :: ReaderT SudokuEnv (StateT Options []) Solution
solutions = do
    env <- ask
    let dimensions = envDimensions env

    solveFromRow dimensions 0

  where
    solveFromRow :: Int -> Int -> ReaderT SudokuEnv (StateT Options []) Solution
    solveFromRow dimensions = solveFromRow'

      where
        solveFromRow' :: Int -> ReaderT SudokuEnv (StateT Options []) Solution
        solveFromRow' row
          | row >= dimensions = return []
          | otherwise         = do
              solvedRow  <- solveRowFromCol' row 0
              solvedRows <- solveFromRow' $ row + 1
              return $ solvedRow : solvedRows

        solveRowFromCol' :: Int -> Int -> ReaderT SudokuEnv (StateT Options []) [Value]
        solveRowFromCol' row col
          | col >= dimensions = return []
          | otherwise         = do
              value     <- tryAllValues (row, col)
              solvedRow <- solveRowFromCol' row $ col + 1
              return $ value : solvedRow

randSolutions :: StdGen -> ReaderT SudokuEnv (StateT Options []) Solution
randSolutions gen = do
    env <- ask
    let dimensions        = envDimensions env
        cellNumList       = [0 .. square dimensions - 1]
        (randCellList, _) = shuffleList cellNumList gen

    aListToSolution <$> solveFromCellList dimensions randCellList

  where
    aListToSolution :: [[(Int, Value)]] -> Solution
    aListToSolution = map $ map snd

    solveFromCellList :: Int -> [Int] -> ReaderT SudokuEnv (StateT Options []) [[(Int, Value)]]
    solveFromCellList dimensions = solveByCellList emptyAList

      where
        getCell :: Int -> Cell
        getCell = getCellFromNum dimensions

        emptyAList :: [[(Int, Value)]]
        emptyAList =
          let emptyRow = zip [0 .. dimensions - 1] (repeat undefined)
          in  replicate dimensions emptyRow

        solveByCellList :: [[(Int, Value)]]
                        -> [Int]
                        -> ReaderT SudokuEnv (StateT Options []) [[(Int, Value)]]
        solveByCellList result []                 = return result
        solveByCellList result (cellNum:cellNums) = do
          let (row, col) = getCell cellNum

          value <- tryAllValues (row, col)
          newResult <- solveByCellList result cellNums
          return $ set2DAList row col value newResult

tryAllValues :: Cell -> ReaderT SudokuEnv (StateT Options []) Value
tryAllValues cell = do
  possibleVals <- gets $ getCellValues cell
  value        <- lift . lift $ possibleVals
  cell `setCellValue` value
  return value



------- Solvers -------

solve :: Puzzle -> Solver -> [Solution]
solve puzzle solver = fst <$> solver puzzle

defaultSolver :: Solver
defaultSolver (Puzzle size board) =
  getSolutions board solutions `runWithSize` size

randSolver :: StdGen -> Solver
randSolver gen (Puzzle size board) =
  getSolutions board (randSolutions gen) `runWithSize` size

runWithSize :: Reader SudokuEnv a -> Int -> a
runWithSize reader size = runReader reader $ initEnv size

getSolutions :: Board
             -> ReaderT SudokuEnv (StateT Options []) Solution
             -> Reader SudokuEnv [(Solution, Options)]
getSolutions board solution = do
    env <- ask
    let values     = envValues     env
        blocks     = envBlocks     env
        dimensions = envDimensions env
        cellsRange = [0 .. dimensions - 1]
        initOpts   = getInitOptions values cellsRange blocks

    return . runWithState initOpts . runWithEnv env $ do
      initBoard board
      solution

  where runWithState = flip runStateT
        runWithEnv   = flip runReaderT

initBoard :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
           => Board -> m ()
initBoard board = sequence_ [ (r, c) `setCellValue` v | (row, r) <- zip board [0..]
                                                      , (val, c) <- zip row   [0..]
                                                      , v <- maybeToList val ]

replaceCellOpts :: Int -> Int -> [Value] -> CellOptions -> CellOptions
replaceCellOpts = replace2DList

replaceOpts :: Int -> Value -> [Cell] -> ValueOptions -> ValueOptions
replaceOpts = set2DAList
