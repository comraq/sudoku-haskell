{-# LANGUAGE FlexibleContexts #-}

module Definition where

import Control.Monad.Plus
import Control.Monad.State
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
type Board  = [[Maybe Value]]
data Puzzle = Puzzle {
  gameSize  :: Int
, gameBoard :: Board
} deriving Show

type Solution = [[Value]]
type Solver   = Puzzle -> [(Solution, Options)]

type Grid = [[Value]]

gridToPuzzle :: Int -> Grid -> Puzzle
gridToPuzzle size = Puzzle size . map (map (valueToMaybe values))
  where
    values :: [Value]
    values = getValues size

valueToMaybe :: [Value] -> Value -> Maybe Value
valueToMaybe values val
    | val `elem` values = Just val
    | otherwise         = Nothing

data SudokuEnv = SudokuEnv {
  envSize       :: Int
, envDimensions :: Int
, envBlocks     :: [[Cell]]
, envValues     :: [Value]
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

valuesRange :: [Value]
valuesRange =  ['0'..'9'] ++ ['A'..'Z']

getValues :: Int -> [Value]
getValues =  (`take` valuesRange) . square

getCellFromNum :: Int -> Int -> Cell
getCellFromNum dimensions cellNum =
  let row = cellNum `div` dimensions
      col = cellNum `mod` dimensions
  in  (row, col)

square :: Int -> Int
square = (^2)

getBlocks :: Int -> [[Cell]]
getBlocks size =  [[ (row + r * size, col + c * size)
                  | row <- blkSize, col <- blkSize ]
                  | r   <- blkSize, c   <- blkSize ]
  where blkSize = [0..size - 1]

blockNum :: Int -> Cell -> Int
blockNum size (r, c) = rowBlkNum + colBlkNum
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
