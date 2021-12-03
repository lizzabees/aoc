module Y2021.Day1
    ( input
    , problem1
    , problem2
    ) where

zipWithSelf :: (a -> a -> b) -> [a] -> [b]
zipWithSelf f xs = zipWith f xs $ tail xs

input :: FilePath -> IO [Int]
input = fmap (map read . lines) . readFile

problem1 :: [Int] -> Int
problem1 = length . filter (== LT) . zipWithSelf compare

problem2 :: [Int] -> Int
problem2 xs = length . filter (== LT) $ zipWithSelf compare triplets
    where triplets = zipWith3 add3 xs (tail xs) (tail $ tail xs)
          add3 a b c = a + b + c
