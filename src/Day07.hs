module Day07 where

import Control.Exception (onException)
import Data.Bifunctor
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import Debug.Trace

parseC :: [String] -> [Int]
parseC = map read

fuelForPosition :: Int -> [Int] -> Int
fuelForPosition n xs = sum $ map (\el -> abs (el - n)) xs

(...) :: Int -> Int -> [Int]
(...) a b
  | a < b = [1 .. (b - a)]
  | b < a = [1 .. (a - b)]
  | otherwise = [0]

fuelForPosition' :: Int -> [Int] -> Int
fuelForPosition' n xs = sum $ map (\el -> sum (el ... n)) xs

bruteForce :: [Int] -> (Int, Int)
bruteForce xs =
  minimumBy (compare `on` fst) $
    map (\el -> (fuelForPosition (head el) xs, head el)) $ group $ sort xs

splitByMedian :: [Int] -> ([[Int]], [[Int]])
splitByMedian xs = first reverse $ splitAt (length xs `div` 2) $ group $ sort xs

calculateBranch :: [[Int]] -> Int -> [Int] -> Int
calculateBranch [] bestGuess orig = bestGuess
calculateBranch positions@(x : xs) bestGuess orig
  | currentGuess > bestGuess = calculateBranch [] bestGuess orig
  | otherwise = calculateBranch xs currentGuess orig
  where
    currentGuess = fuelForPosition' (head x) orig

day07 :: IO ()
day07 = do
  content <- splitOn "," . head . lines <$> readFile "./src/inputs/day07.txt"
  -- part A
  let input = sort $ parseC content
  print $ bruteForce input

  -- part B
  -- left and right branch of binary tree
  let (l, r) = splitByMedian [minimum input .. maximum input]
  let x1 = calculateBranch l (maxBound :: Int) input
  let x2 = calculateBranch r (maxBound :: Int) input
  print $ min x1 x2
