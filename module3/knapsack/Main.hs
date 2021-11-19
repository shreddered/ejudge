module Main where

import Data.Array
import Data.List (intercalate)

-- | Create table of thunks
createTable weights prices capacity n = arr
  where
    -- An actual table (which is computed lazily)
    arr = array ((0, 0), (n, capacity))
                [((i, j), best i j) | i <- [0..n], j <- [0..capacity]]
    -- A decision making process
    best _ 0 = 0
    best 0 _ = 0
    best i j
      | weights ! i > j = arr ! (i - 1, j)
      | otherwise       =
          max (arr ! (i - 1, j)) (prices ! i + arr ! (i - 1, j - weights ! i))

-- | Restore items
restorePath _ 0 _ _         = []
restorePath _ _ 0 _         = []
restorePath arr i j weights
  | arr ! (i - 1, j) == arr ! (i, j) = restorePath arr (i - 1) j weights
  | otherwise                        =
      i : (restorePath arr (i - 1) (j - weights ! i) weights)

restoreWeightAndPath arr n capacity weights = (foldr (+) 0 weights', path)
  where
    path = reverse (restorePath arr n capacity weights)
    weights' = map ((!) weights) path

type Item = (Int, Int)

knapsack :: [Item] -> Int -> (Int, Int, [Int])
knapsack items capacity = (weight, price, path)
  where
    n = length items
    (weights, prices) = unzip items
    -- Create arrays for O(1) indexing
    weights' = listArray (1, n) weights
    prices'  = listArray (1, n) prices

    table = createTable weights' prices' capacity n
    price = table ! (n, capacity)
    (weight, path) = restoreWeightAndPath table n capacity weights'

main :: IO ()
main = interact (unlines . processInput . filter (not . null) . lines)

-- TODO: error handling
processInput :: [String] -> [String]
processInput [] = []
processInput xs = (intercalate " " (map show [weight, price])) : (map show path)
  where
    capacity = read (head xs)
    splitLines = map words (tail xs)
    items = map (\ [x, y] -> ((read x :: Int), (read y :: Int))) splitLines
    (weight, price, path) = knapsack items capacity
