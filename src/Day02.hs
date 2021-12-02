module Day02 where

data Position = Position
  { positionHorizontal :: Int,
    positionDepth :: Int,
    positionAim :: Int
  }
  deriving (Show)

instance Semigroup Position where
  Position h1 d1 a1 <> Position h2 d2 a2 =
    Position (h1 + h2) (d1 + d2 + a1 * h2) (a1 + a2)

instance Monoid Position where
  mempty = Position 0 0 0

commandToPosition :: String -> Position
commandToPosition s = case words s of
  ["forward", n] -> Position (read n) 0 0
  ["up", n] -> Position 0 (negate $ read n) 0
  ["down", n] -> Position 0 (read n) 0
  _ -> Position 0 0 0

commandToPosition' :: String -> Position
commandToPosition' s = case words s of
  ["forward", n] -> Position (read n) 0 0
  ["up", n] -> Position 0 0 (negate $ read n)
  ["down", n] -> Position 0 0 (read n)
  _ -> Position 0 0 0

solve :: Position -> Int
solve (Position h d _) = h * d

day02 :: IO ()
day02 = do
  content <- lines <$> readFile "./src/inputs/day02.txt"
  -- solution 1
  print $ solve $ mconcat $ map commandToPosition content
  -- solution 2
  print $ solve $ mconcat $ map commandToPosition' content
