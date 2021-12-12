module Day01 where

import Control.Applicative
import Data.List

windows :: Int -> [a] -> [[a]]
windows n = getZipList . traverse ZipList . take n . tails

countIncreases :: [Int] -> Int
countIncreases = sum . map fn . windows 2
  where
    fn [f, s]
      | s > f = 1
      | otherwise = 0
    fn _ = 0

rollingMeasurement :: [Int] -> [Int]
rollingMeasurement = map sum . windows 3

day01 :: IO ()
day01 = do
  input <- map read . lines <$> readFile "./src/inputs/day01.txt"
  -- part 1 solution
  print $ countIncreases input
  -- part 2 solution
  print $ (countIncreases . rollingMeasurement) input
