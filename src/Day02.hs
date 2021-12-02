module Day02 where

calculatePosition :: [String] -> (Int, Int) -> (Int, Int)
calculatePosition input acc = foldl parseP acc input
  where
    parseP (horizontal, vertical) s = case words s of
      ["forward", n] -> (horizontal + read n, vertical)
      ["up", n] -> (horizontal, vertical - read n)
      ["down", n] -> (horizontal, vertical + read n)
      _ -> (horizontal, vertical)

solve1 :: (Int, Int) -> Int
solve1 (h, v) = h * v

calculatePosition2 :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
calculatePosition2 input acc = foldl parseP acc input
  where
    parseP (horizontal, vertical, aim) s = case words s of
      ["forward", n] -> (horizontal + read n, f aim n, aim)
        where
          f 0 _ = vertical
          f x n = vertical + read n * aim
      ["down", n] -> (horizontal, vertical, aim + read n)
      ["up", n] -> (horizontal, vertical, aim - read n)
      _ -> (horizontal, vertical, aim)

solve2 :: (Int, Int, Int) -> Int
solve2 (h, v, _) = h * v

day02 :: IO ()
day02 = do
  content <- lines <$> readFile "./src/inputs/day02.txt"
  print $ solve1 $ calculatePosition content (0, 0)
  print $ solve2 $ calculatePosition2 content (0, 0, 0)
