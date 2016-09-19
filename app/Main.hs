module Main where

import System.Random (getStdGen, split, setStdGen)

import Definition
import Solver
import Generator

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

main :: IO ()
main = do
    gen <- getStdGen
    let size       = 3
        (g1, g2)   = split gen
        norm       = generatePuzzle g1 size Normal
        hard       = generatePuzzle g2 size Hard
        solver     = defaultSolver

    putStrLn $ "Normal Puzzle: " ++ show norm
    putStrLn $ "Count of Nothing: " ++ show (count norm)
    print    $ head $ solve norm solver

    putStrLn $ "Hard Puzzle: " ++ show hard
    putStrLn $ "Count of Nothing: " ++ show (count hard)
    print    $ head $ solve hard solver

    setStdGen g2

  where
    count :: Puzzle -> Int
    count (Puzzle _ board) = length . filter (== Nothing) . concat $ board
