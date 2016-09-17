{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( Solution
  , Puzzle
  , Value
  , getValues
  , solve
  , empty
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

-- Each Sudoku Cell Contains a Char
type Value  = Char

{-
 - Each Cell is of coordinates where:
 - - (0, 0) being the top left
 - - (8, 8) being the bottom right
 -   in a typical 3 x 3 (size 3 sudoku)
 -}
type Cell  = (Int, Int)

-- 2D list of size (dimensions * dimensions)
type Puzzle   = [[Maybe Value]]
type Solution = [[Value]]


valuesRange :: [Value]
valuesRange =  ['0'..'9'] ++ ['A'..'Z']

getValues :: Int -> [Value]
getValues =  (`take` valuesRange) . square

square :: Int -> Int
square = (id &&& id) >>> uncurry (*)

getBlocks :: Int -> [[Cell]]
getBlocks size =  [[ (row + r * size, col + c * size )
                  | row <- blkSize, col <- blkSize ]
                  | r   <- blkSize, c   <- blkSize ]
  where blkSize = [0..size - 1]

blockNum' :: Int -> Cell -> Int
blockNum' size (r, c) = rowBlkNum + colBlkNum
  where colBlkNum = c `div` size
        rowBlkNum = (r `div` size) * size

type CellOptions  = [[[Value]]]         -- list of possible values for each cell
type ValueOptions = [[(Value, [Cell])]]
type RowOptions   = ValueOptions        -- list of rows of values mapping to cells
type ColOptions   = ValueOptions        -- list of cols of values mapping to cells
type BlkOptions   = ValueOptions        -- list of blks of values mapping to cells

data Options = Options {
  cellOpts :: CellOptions
, rowOpts  :: RowOptions
, colOpts  :: ColOptions
, blkOpts  :: BlkOptions
} deriving Show

modifyCellOpts :: (MonadState Options m, MonadPlus m) => (CellOptions -> CellOptions) -> m ()
modifyCellOpts f = do
  options <- get
  put $ options { cellOpts = f $ cellOpts options }

modifyRowOpts :: (MonadState Options m, MonadPlus m) => (RowOptions -> RowOptions) -> m ()
modifyRowOpts f = do
  options <- get
  put $ options { rowOpts = f $ rowOpts options }

modifyColOpts :: (MonadState Options m, MonadPlus m) => (ColOptions -> ColOptions) -> m ()
modifyColOpts f = do
  options <- get
  put $ options { colOpts = f $ colOpts options }

modifyBlkOpts :: (MonadState Options m, MonadPlus m) => (BlkOptions -> BlkOptions) -> m ()
modifyBlkOpts f = do
  options <- get
  put $ options { blkOpts = f $ blkOpts options }

getInitOptions :: [Value] -> [Int] -> [[Cell]] -> Options
getInitOptions values cellsRange blocks = Options {
      cellOpts = [[ values       | _     <- cellsRange ]
                                 | _     <- cellsRange ]

    , rowOpts  = [[ (v, [ (r, c) | c     <- cellsRange ])
                                 | v     <- values     ]
                                 | r     <- cellsRange ]

    , colOpts  = [[ (v, [ (r, c) | r     <- cellsRange ])
                                 | v     <- values     ]
                                 | c     <- cellsRange ]

    , blkOpts  = [[ (v, block)   | v     <- values     ]
                                 | block <- blocks     ]
    }

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
    let size       = size' env
        dimensions = dimensions' env
        blocks     = blocks' env
        values     = values' env

        blk        = blockNum' size cell
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
 -}
constrainCell :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
              => Value
              -> Cell
              -> m ()
constrainCell val cell@(row, col) = do
    env <- ask
    let size = size' env
        blk  = blockNum' size cell

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

solutions :: ReaderT SudokuEnv (StateT Options []) Solution
solutions = do
    env <- ask
    let dimensions = dimensions' env

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
              possibleVals <- gets $ getCellValues (row, col)
              value        <- lift . lift $ possibleVals

              (row, col) `setCellValue` value
              solvedRow <- solveRowFromCol' row $ col + 1
              return $ value : solvedRow

getSolutions :: Puzzle
             -> (StateT Options [] Solution -> Options -> b)
             -> Reader SudokuEnv b
getSolutions puz runStateFunc = do
    env <- ask
    let values     = values'     env
        blocks     = blocks'     env
        dimensions = dimensions' env
        cellsRange = [0 .. dimensions - 1]
        initOpts   = getInitOptions values cellsRange blocks

    return . runWithState initOpts . runWithEnv env $ do
      initPuzzle puz
      solutions

  where runWithState = flip runStateFunc
        runWithEnv   = flip runReaderT

initPuzzle :: (MonadReader SudokuEnv m, MonadState Options m, MonadPlus m)
           => Puzzle -> m ()
initPuzzle puz = sequence_ [ (r, c) `setCellValue` v | (row, r) <- zip puz [0..]
                                                     , (val, c) <- zip row [0..]
                                                     , v <- maybeToList val ]

solveWithOptions :: Int -> Puzzle -> [(Solution, Options)]
solveWithOptions size puzzle = getSolutions puzzle runStateT `runWithSize` size

runWithSize :: Reader SudokuEnv a -> Int -> a
runWithSize reader size = runReader reader $ initEnv size

solve :: Int -> Puzzle -> [Solution]
solve size puzzle = fst <$> solveWithOptions size puzzle

empty :: Int -> [(Solution, Options)]
empty size = solveWithOptions size []



data SudokuEnv = SudokuEnv {
  size'       :: Int
, dimensions' :: Int
, blocks'     :: [[Cell]]
, values'     :: [Value]
} deriving Show

initEnv :: Int -> SudokuEnv
initEnv size = SudokuEnv size dim blocks values
  where
    dim :: Int
    dim = square size

    blocks :: [[Cell]]
    blocks = getBlocks size

    values :: [Value]
    values = getValues size



{-
 - Helper Setter Functions
 -
 - TODO: Consider using lens setters
 -}
replaceCellOpts :: Int -> Int -> [Value] -> CellOptions -> CellOptions
replaceCellOpts 0   col newVals (vs:vss) = replaceInList col newVals vs : vss
replaceCellOpts row col newVals (vs:vss) = vs : replaceCellOpts (row - 1) col newVals vss

replaceInList :: Int -> a -> [a] -> [a]
replaceInList 0 newX (_:xs) = newX:xs
replaceInList n newX (x:xs) = x : replaceInList (n - 1) newX xs

replaceOpts :: Int -> Value -> [Cell] -> ValueOptions -> ValueOptions
replaceOpts 0 value cells (pr:prs) = setAssocList (== value) cells pr : prs
replaceOpts n value cells (pr:prs) = pr : replaceOpts (n - 1) value cells prs

setAssocList :: (k -> Bool) -> v -> [(k, v)] -> [(k, v)]
setAssocList predicate newVal ((key, value):prs)
  | predicate key = (key, newVal) : prs
  | otherwise     = (key, value)  : setAssocList predicate newVal prs