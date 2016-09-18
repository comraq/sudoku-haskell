module Main where

import System.Random (getStdGen, split, setStdGen)

import Lib
import Definition

type Grid = [[Value]]

h1 :: Grid
h1 = [ "4.....8.5"
     , ".3......."
     , "...7....."
     , ".2.....6."
     , "....8.4.."
     , "....1...."
     , "...6.3.7."
     , "5..2....."
     , "1.4......"
     ]

gridToPuzzle :: Int -> Grid -> Puzzle
gridToPuzzle size = Puzzle size . map (map (valueToMaybe values))
  where
    values :: [Value]
    values = getValues size

valueToMaybe :: [Value] -> Value -> Maybe Value
valueToMaybe values val
    | val `elem` values = Just val
    | otherwise         = Nothing

main :: IO ()
main = do
  randGen <- getStdGen

  let size     = 3
      board    = []
      puzzle   = Puzzle size board

      (g1, g2) = split randGen
      solver   = randSolver g1
      solution = solve puzzle solver

  print $ head solution
  setStdGen g2
