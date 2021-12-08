module Day08 where

import Data.List.Split (splitOn)

type SignalData = (String, String)

splitInputOutput :: String -> SignalData
splitInputOutput s =
  let [input, output] = splitOn " | " s
   in (input, output)

countUniqueDigits :: [String] -> Int
countUniqueDigits = sum . map countDigit
  where
    countDigit el
      | length el `elem` [2, 3, 4, 7] = 1
      | otherwise = 0

calculateUniqueSegmentDigits :: [SignalData] -> Int
calculateUniqueSegmentDigits = sum . map (countUniqueDigits . words . snd)

day08 :: IO ()
day08 = do
  content <- map splitInputOutput . lines <$> readFile "./src/inputs/day08.txt"
  -- part A
  print $ calculateUniqueSegmentDigits content
