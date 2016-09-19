{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Test.QuickCheck

import Definition
import Solver

import Control.Monad(forM_)

{-
 - Constrain the domain of Int to between 3 to 5 for Sudoku Block Sizes
 -
 - @link - https://www.reddit.com/r/haskellquestions/comments/rbctw/restricting_quickcheck_int_input/
 -}

defaultOpts :: Args
defaultOpts = stdArgs

type Run = Args -> IO ()

run :: Testable prop => prop -> Run
run = flip quickCheckWith

runTests :: String -> Args -> [Run] -> IO ()
runTests name opts tests =
  putStrLn ("Running " ++ name ++ " tests:")
  >> forM_ tests (\rn -> rn opts)

main :: IO ()
main = do
  runTests "default" defaultOpts
    [ run prop_
    ]

newtype SizeInt = SizeInt Int
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)

instance Arbitrary SizeInt where
  arbitrary = fmap SizeInt (choose (3, 5) :: Gen Int)

prop_ :: SizeInt -> Bool
prop_ (SizeInt n) = length (getValues n) == n * n

