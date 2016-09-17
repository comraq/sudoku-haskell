module Main where

import Lib

type Grid = [[Value]]

h1 :: [[Value]]
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
gridToPuzzle size = map $ map (valueToMaybe values)
  where
    values :: [Value]
    values = getValues size

valueToMaybe :: [Value] -> Value -> Maybe Value
valueToMaybe values val
    | val `elem` values = Just val
    | otherwise         = Nothing

main :: IO ()
main = do
  let size     = 3
      puzzle   = gridToPuzzle size h1
      solution = solve size puzzle

  print solution
