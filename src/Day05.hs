{-# LANGUAGE TupleSections #-}

module Day05 where

import Control.Applicative (Applicative (liftA2))
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import Data.Map (fromListWith, fromListWithKey, intersectionWith, unionWith)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Sequence (fromList, mapWithIndex, update)
import qualified Data.Set as S

type Coordinate = (Int, Int)

type Line = (Coordinate, Coordinate)

parseLineCoord :: String -> Line
parseLineCoord s = ((x1, y1), (x2, y2))
  where
    numbers = splitOn " " s
    [x1, y1] = map read $ splitOn "," $ head numbers
    [x2, y2] = map read $ splitOn "," $ last numbers

safeSpread :: Int -> Int -> [Int]
safeSpread x y = if x < y then [x .. y] else reverse [y .. x]

getLineCoord :: Line -> [Coordinate]
getLineCoord ((x1, y1), (x2, y2))
  -- vertical
  | x1 == x2 = map (x1,) $ safeSpread y1 y2
  -- horizontal
  | y1 == y2 = map (,y1) $ safeSpread x1 x2
  -- diagonal
  | otherwise = zip (safeSpread x1 x2) (safeSpread y1 y2)

solve :: [[Coordinate]] -> Int
solve = length . filter ((> 1) . length) . group . sort . concat

day05 :: IO ()
day05 = do
  contents <- map (getLineCoord . parseLineCoord) . lines <$> readFile "./src/inputs/day05.txt"
  print $ solve contents
