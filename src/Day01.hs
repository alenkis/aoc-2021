module Day01 where

import Control.Applicative
import Data.List

windows :: Int -> [a] -> [[a]]
windows n = getZipList . traverse ZipList . take n . tails

countIncreases :: [Int] -> Int
countIncreases input = (sum . map fn) (windows 2 input)
  where
    fn [f, s]
      | s > f = 1
      | otherwise = 0
    fn _ = 0

rollingMeasurement :: [Int] -> [Int]
rollingMeasurement input = map fn $ zip3 (tail . tail $ input) (tail input) input
  where
    fn (f, s, t) = f + s + t

rollingMeasurement' :: [Int] -> [Int]
rollingMeasurement' input = map fn $ windows 3 input
  where
    fn [f, s, t] = f + s + t
    fn _ = 0

day01 :: IO ()
day01 = do
  input <- map read . lines <$> readFile "./src/inputs/day01.txt"
  -- part 1 solution
  print $ countIncreases input
  -- part 2 solution
  print $ (countIncreases . rollingMeasurement) input
