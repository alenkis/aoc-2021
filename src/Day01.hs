module Day01 where

countIncreases :: [Int] -> Int
countIncreases input = sum $ zipWith (curry fn) (tail input) input
  where
    fn (f, s)
      | f > s = 1
      | otherwise = 0

rollingMeasurement :: [Int] -> [Int]
rollingMeasurement input = map fn $ zip3 (tail (tail input)) (tail input) input
  where
    fn (f, s, t) = f + s + t

day01 :: IO ()
day01 = do
  input <- map read . lines <$> readFile "./src/inputs/day01.txt"
  -- part 1 solution
  print $ countIncreases input
  -- part 2 solution
  print $ countIncreases $ rollingMeasurement input
