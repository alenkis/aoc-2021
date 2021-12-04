module Day03 where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (foldl')

newtype Binary = Binary [Int]
  deriving (Show)

instance Semigroup Binary where
  Binary s1 <> Binary s2 = Binary $ map (uncurry (+)) $ zip s1 s2

instance Monoid Binary where
  mempty = Binary (repeat 0)

getRates :: [Binary] -> (Int, Int)
getRates bs =
  let gamma = f (mconcat bs) (length bs)
        where
          f (Binary d) l = map (\el -> if el > l `div` 2 then '1' else '0') d
   in (toDec gamma, toDec $ reverseBinary gamma)

reverseBinary :: String -> String
reverseBinary = map f
  where
    f '0' = '1'
    f _ = '0'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

solve :: (Int, Int) -> Int
solve (gamma, epsilon) = gamma * epsilon

day03 :: IO ()
day03 = do
  contents <- map (Binary . map digitToInt) . lines <$> readFile "./src/inputs/day03.txt"
  print $ solve $ getRates contents
