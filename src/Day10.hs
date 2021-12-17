module Day10 where

import Control.Monad (foldM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (isRight)
import Data.Function (on)
import Data.List (sortBy)

getMatchingPair :: Char -> Char
getMatchingPair '{' = '}'
getMatchingPair '}' = '{'
getMatchingPair '(' = ')'
getMatchingPair ')' = '('
getMatchingPair '<' = '>'
getMatchingPair '>' = '<'
getMatchingPair '[' = ']'
getMatchingPair ']' = '['
getMatchingPair _ = error "not implemented"

walkAndCancel :: String -> String
walkAndCancel = foldl f ""
  where
    f [] el = [el]
    f [x] el = if shouldCancelOut x el then [] else x : [el]
    f acc el = if shouldCancelOut (last acc) el then init acc else acc ++ [el]
    shouldCancelOut ']' '[' = True
    shouldCancelOut '}' '{' = True
    shouldCancelOut '>' '<' = True
    shouldCancelOut ')' '(' = True
    shouldCancelOut _ _ = False

isClosing :: Char -> Bool
isClosing c = c `elem` ['}', ')', '>', ']']

isOpening :: Char -> Bool
isOpening = not . isClosing

solve :: String -> Either Char Char
solve = foldM f '_'
  where
    f b a =
      if isOpening a
        then Left (getMatchingPair a) -- Found corruped chunk
        else Right a

sumCorruptedLines xs = sum $ map f xs
  where
    f (Right _) = 0
    f (Left found) = getPoints found
    getPoints ')' = 3
    getPoints ']' = 57
    getPoints '}' = 1197
    getPoints '>' = 25137
    getPoints _ = 0

getClosingSequences = map getClosingSequence
  where
    getClosingSequence = reverse . walkAndCancel . map getMatchingPair

discardCorruptedLines :: [String] -> [String]
discardCorruptedLines = filter fn
  where
    fn = isRight . solve . walkAndCancel . map getMatchingPair

scoreCompletion :: String -> (String, Int)
scoreCompletion s = (s, foldl fn 0 s)
  where
    fn acc el = acc * 5 + bracketValue el
    bracketValue ')' = 1
    bracketValue ']' = 2
    bracketValue '}' = 3
    bracketValue '>' = 4
    bracketValue _ = 0

sortCompletions :: [(String, Int)] -> [(String, Int)]
sortCompletions = sortBy (compare `on` snd)

day10 :: IO ()
day10 = do
  contents <- lines <$> readFile "./src/inputs/day10.txt"
  -- Part 1
  print $ sumCorruptedLines . map (solve . walkAndCancel . map getMatchingPair) $ contents
  let sorted = (sortCompletions . map scoreCompletion . getClosingSequences . discardCorruptedLines) contents
  -- Part 2
  print $ snd $ sorted !! (length sorted `div` 2)
