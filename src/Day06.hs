module Day06 where

import Data.List
import Data.List.Split (splitOn)
import Prelude

countListComp :: Eq a => [a] -> a -> Int
countListComp [] find = 0
countListComp ys find = length xs
  where
    xs = [xs | xs <- ys, xs == find]

frequencyList :: [Int] -> [Int]
frequencyList xs = map (\idx -> if idx `elem` sorted then occurence idx else 0) [0 .. 8]
  where
    sorted = sort xs
    occurence n = countListComp sorted n

rotateList :: [Int] -> [Int]
rotateList l = zipWith const (drop 1 $ cycle l) l

updateFish :: [Int] -> [Int]
updateFish xs = zipWith (\el idx -> (if idx == 6 then last xs + el else el)) xs [0 .. 8]

solveForDays :: Int -> [Int] -> [Int]
solveForDays 0 initial = initial
solveForDays days initial = solveForDays (days - 1) (updateFish $ rotateList initial)

day06 :: IO ()
day06 = do
  initialFrequency <- frequencyList . parseNumbers <$> readFile "./src/inputs/day06.txt"
  -- part 1
  print $ sum $ solveForDays 80 initialFrequency
  -- part 2
  print $ sum $ solveForDays 256 initialFrequency

parseNumbers :: String -> [Int]
parseNumbers = map read . splitOn ","
