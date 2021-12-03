module Y2021.Day2
    ( Move(..)
    , input 
    , problem1
    , problem2
    ) where

import Data.List (foldl')
data Move = Forward Int
          | Down Int
          | Up Int
    deriving (Eq,Show)

input :: FilePath -> IO [Move]
input = fmap (map (readMove . words) . lines) . readFile
    where readMove ["forward", del] = Forward $ read del
          readMove ["down",    del] = Down $ read del
          readMove ["up",      del] = Up $ read del

move1 :: (Int, Int) -> Move -> (Int, Int)
move1 (px, py) (Forward dx) = (px + dx, py)
move1 (px, py) (Down    dy) = (px, py + dy) 
move1 (px, py) (Up      dy) = (px, py - dy)

problem1 :: [Move] -> Int
problem1 = inner . foldl' move1 (0, 0)
    where inner :: (Int, Int) -> Int
          inner (x, y) = x * y

move2 :: (Int, Int, Int) -> Move -> (Int, Int, Int)
move2 (px, py, aim) (Forward dx) = (px + dx, py + aim * dx, aim)
move2 (px, py, aim) (Down    dx) = (px     , py      , aim + dx)
move2 (px, py, aim) (Up      dx) = (px     , py      , aim - dx)

problem2 :: [Move] -> Int
problem2 = inner . foldl' move2 (0, 0, 0)
    where inner :: (Int, Int, Int) -> Int
          inner (x, y, _) = x * y
