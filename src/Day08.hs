module Day08 where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import GHC.Float (expts10)

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

-- part 2

-- Given two strings, it either returns true if s1 contains all characters from s2
-- or returns how many characters it contains
elementOf :: String -> String -> Either Int Bool
elementOf s1 s2 =
  if diff == s1
    then return True
    else
      ( case length diff of
          0 -> return False
          n -> Left n
      )
  where
    diff = s1 `intersect` s2

findDigit :: Int -> [String] -> String
findDigit 7 xs = fromMaybe "" $ find ((== 3) . length) xs
findDigit 4 xs = fromMaybe "" $ find ((== 4) . length) xs
findDigit _ _ = ""

mapDigits :: [String] -> [Maybe (String, Int)]
mapDigits xs = map f xs
  where
    f s = case length s of
      2 -> return (s, 1)
      3 -> return (s, 7)
      4 -> return (s, 4)
      5 -> case findDigit 7 xs `elementOf` s of
        Right True -> return (s, 3)
        _ -> case findDigit 4 xs `elementOf` s of
          Left 3 -> return (s, 5)
          _ -> return (s, 2)
      6 -> case findDigit 4 xs `elementOf` s of
        Right True -> return (s, 9)
        _ -> case findDigit 7 xs `elementOf` s of
          Right True -> return (s, 0)
          _ -> return (s, 6)
      7 -> return (s, 8)
      _ -> Nothing

decodeSignals :: [SignalData] -> Int
decodeSignals = sum . map f
  where
    f (input, output) =
      let dictionaryList = (catMaybes . mapDigits . words) input
       in ( read
              . concatMap (show . snd)
              . mapMaybe (`decodeFromDict` dictionaryList)
              . words
          )
            output
    decodeFromDict s = find ((==) (sort s) . sort . fst)

day08 :: IO ()
day08 = do
  content <- map splitInputOutput . lines <$> readFile "./src/inputs/day08.txt"
  -- part A
  print $ calculateUniqueSegmentDigits content
  -- part B
  print $ decodeSignals content
