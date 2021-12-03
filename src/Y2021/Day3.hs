module Y2021.Day3
    ( input
    , problem1
    , problem2
    , testData
    ) where

import Data.Char (digitToInt)
import Data.List (transpose)

input :: FilePath -> IO [[Int]]
input = fmap (map (map digitToInt) . lines) . readFile

mcb :: [Int] -> Int
mcb xs = if avg >= 0.5
            then 1
            else 0
                where avg = fromIntegral (sum xs) / fromIntegral (length xs)

lcb :: [Int] -> Int
lcb xs = 1 - mcb xs

binDigitsToInt :: [Int] -> Int
binDigitsToInt = fst . foldr iter (0,0)
    where iter :: Int -> (Int, Int) -> (Int, Int)
          iter x (total, pow) = (total + x * 2 ^ pow, pow + 1)

problem1 :: [[Int]] -> Int
problem1 xs = fn mcb xs * fn lcb xs
    where fn f = binDigitsToInt . map f . transpose

filterOn :: ([Int] -> Int) -> [[Int]] -> [Int]
filterOn f xs = go 0 xs
    where
        go :: Int -> [[Int]] -> [Int]
        go n xs = let bit = f . (!! n) $ transpose xs in
                      case filter ((== bit) . (!! n)) xs of
                        [x] -> x
                        rem -> go (n + 1) rem

problem2 :: [[Int]] -> Int
problem2 xs = fn mcb xs * fn lcb xs
    where fn f = binDigitsToInt . filterOn f

testData :: [[Int]]
testData =
    [ [0,0,1,0,0]
    , [1,1,1,1,0]
    , [1,0,1,1,0]
    , [1,0,1,1,1]
    , [1,0,1,0,1]
    , [0,1,1,1,1]
    , [0,0,1,1,1]
    , [1,1,1,0,0]
    , [1,0,0,0,0]
    , [1,1,0,0,1]
    , [0,0,0,1,0]
    , [0,1,0,1,0]
    ]
