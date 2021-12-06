module Day04 where

import Control.Applicative
import Data.Char (GeneralCategory)
import Data.Foldable (Foldable (foldl'))
import Data.List
import Data.List.Split
import Debug.Trace (trace)

type Number = (Int, Bool)

type Board = [[Number]]

markNumber :: Int -> Number -> Number
markNumber drawn (n, isMarked)
  | n == drawn = (n, True)
  | otherwise = (n, isMarked)

markRow :: Int -> [Number] -> [Number]
markRow drawnNumber = map $ markNumber drawnNumber

markBoardForN :: Int -> Board -> Board
markBoardForN drawnN = map $ markRow drawnN

markBoard :: Board -> [Int] -> Board
markBoard = foldl' $ flip markBoardForN

isRowComplete :: [Number] -> Bool
isRowComplete = all snd

transposeBoard :: Board -> Board
transposeBoard board = getZipList $ traverse ZipList board

didBoardWin :: Board -> Bool
didBoardWin board = any isRowComplete board || any isRowComplete (transposeBoard board)

getWinningBoard :: [Board] -> Maybe Board
getWinningBoard = find didBoardWin

getWinningBoardIdxs' :: [Board] -> Maybe [Int]
getWinningBoardIdxs' b = case findIndices didBoardWin b of
  [] -> Nothing
  idxs -> Just idxs

generateNewBoardStates :: [Board] -> Int -> [Board]
generateNewBoardStates boards n = map (markBoardForN n) boards

solve :: [Board] -> [Int] -> (Board, Int)
solve _ [] = error "no winning board"
solve boards (n : ns) =
  case winningBoard of
    Just b -> (b, n)
    Nothing -> solve newBoardStates ns
  where
    newBoardStates = generateNewBoardStates boards n
    winningBoard = getWinningBoard newBoardStates

sumNonMarkedNumbers :: Board -> Int
sumNonMarkedNumbers = sum . concatMap (map (\(n, isMarked) -> if isMarked then 0 else n))

-- part 2

filterIndexed :: (a -> Int -> Bool) -> [a] -> [a]
filterIndexed p xs = [x | (x, i) <- zip xs [0 ..], p x i]

removeAtIdxs :: [Int] -> [a] -> [a]
removeAtIdxs idxs = filterIndexed (\el idx -> idx `notElem` idxs)

solveForLast :: [Board] -> [Int] -> (Board, Int)
solveForLast _ [] = error "no winning board"
solveForLast boards (n : ns) =
  case (winningBoardIdx, newBoardStates) of
    -- there's a winning board index (bingo!) and only one board
    (Just wIdx, [lastBoard]) -> (newBoardStates !! head wIdx, n)
    -- there's a winning board (bingo!) and more than one board, so recurse
    (Just idxs, _ : _) -> solveForLast (removeAtIdxs idxs newBoardStates) ns
    -- no winning numbers, try new number
    _ -> solveForLast newBoardStates ns
  where
    newBoardStates = generateNewBoardStates boards n
    winningBoardIdx = getWinningBoardIdxs' newBoardStates

day04 :: IO ()
day04 = do
  (numbers, boards) <- parseNumbersAndBoards . lines <$> readFile "./src/inputs/day04.txt"
  -- part 1
  let (winningBoard, lastN) = solve boards numbers
  print $ sumNonMarkedNumbers winningBoard * lastN
  -- part 2
  let (lastWinningBoard, lastNumber) = solveForLast boards numbers
  print $ sumNonMarkedNumbers lastWinningBoard * lastNumber
  print lastNumber

parseBoards :: [String] -> [Board]
parseBoards input = map (map (map (pairN . read) . words)) $ chunksOf 5 input
  where
    pairN el = (el, False)

parseNumbersAndBoards :: [String] -> ([Int], [Board])
parseNumbersAndBoards input = (numbers, boards)
  where
    numbers = map read $ splitOn "," . head $ input
    boards = parseBoards $ filter (not . null) $ tail input
