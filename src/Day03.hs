module Day03 where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (foldl')
import Debug.Trace (trace)

newtype Binary = Binary [Int]
  deriving (Show)

instance Semigroup Binary where
  Binary s1 <> Binary s2 = Binary $ zipWith (+) s1 s2

instance Monoid Binary where
  mempty = Binary (repeat 0)

data Digits = Zero | One

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

-- Part 2

getElementsByBitCriteria :: [Binary] -> Digits -> [Int]
getElementsByBitCriteria bs digits = f $ mconcat bs
  where
    f (Binary b) = map (criteria $ length bs) b
    criteria l oneCount = case digits of
      Zero -> if oneCount >= (l - oneCount) then 0 else 1
      One -> if oneCount >= (l - oneCount) then 1 else 0

filterByCriteria :: Int -> [Binary] -> Digits -> Binary
filterByCriteria _ [b] _ = b
filterByCriteria initialPos input digits =
  let newData =
        filter
          (\(Binary el) -> el !! initialPos == getElementsByBitCriteria input digits !! initialPos)
          input
   in filterByCriteria (initialPos + 1) newData digits

binToDec :: Binary -> Int
binToDec (Binary b) = toDec $ concatMap show b

day03 :: IO ()
day03 = do
  contents <- map (Binary . map digitToInt) . lines <$> readFile "./src/inputs/day03.txt"
  -- Part 1
  print $ solve $ getRates contents

  -- Part 2
  let oxygen = binToDec $ filterByCriteria 0 contents One
  let co2 = binToDec $ filterByCriteria 0 contents Zero
  print $ oxygen * co2
