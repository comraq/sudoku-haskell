module Util
  ( replace2DList
  , set2DAList
  ) where

-- TODO: Consider using lens setters

replace2DList :: Int -> Int -> a -> [[a]] -> [[a]]
replace2DList 0   col newX (xs:xss) = replaceInList col newX xs : xss
replace2DList row col newX (xs:xss) = xs : replace2DList (row - 1) col newX xss
replace2DList r   c   _    []       = error $ "r: " ++ show r ++ ", c: " ++ show c

replaceInList :: Int -> a -> [a] -> [a]
replaceInList 0 newX (_:xs) = newX:xs
replaceInList n newX (x:xs) = x : replaceInList (n - 1) newX xs

set2DAList :: Eq k => Int -> k -> v -> [[(k, v)]] -> [[(k, v)]]
set2DAList 0 key value (pr:prs) = setAssocList (== key) value pr : prs
set2DAList n key value (pr:prs) = pr : set2DAList (n - 1) key value prs

setAssocList :: (k -> Bool) -> v -> [(k, v)] -> [(k, v)]
setAssocList predicate newVal (pr@(key, _):prs)
  | predicate key = (key, newVal) : prs
  | otherwise     = pr : setAssocList predicate newVal prs
