{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Implements a Raskell solution to the first day of Advent of Code 2023.
--
-- In this puzzle, we need to extract numbers (calibration values) from strings.
-- Then we need to sum them.
--
-- The calibration values are two-digit numbers, given by the first and
-- last digits that appear in the string.
--
-- In the first half of the puzzle, a digit is a literal digit.
-- In the second half, a digit can also be spelled out in letters.
--
-- The pure RASP-L program handles the selection of first and last _literal_ digits,
-- which solves the first half of the puzzle.
-- Matching strings of letters to digits as required in the second half of the puzzle
-- is treated as part of tokenization, which is handled by Haskell.
--
-- We do not add up the calibration values using RASP-L,
-- though this could be done using the addition routines
-- from the
-- [What Algorithms Can Transformers Learn](https://arxiv.org/abs/2310.16028)
-- paper.
module Lib
  ( parseInput,
    extractCalVal,
    sumCalVals,
    tokenize,
    tokenize',
    showSequence,
    showToken,
  )
where

import Data.Char (isDigit, isLower, ord)
import Data.List (isPrefixOf)
import RaskellCore
import RaskellLib
import Text.Printf (printf)

type CalibrationValue = [Token]

-- | Extracts the calibration value from a list of tokens
-- The calibration value is a two-digit number, represented as two tokens.
-- We do autoregressive "sampling" for two steps to extract the calibration value.
extractCalVal :: [Token] -> CalibrationValue
extractCalVal ts = extract $ sample (-127) raspCalVals ts 2
  where
    extract :: [Token] -> [Token]
    extract outs = [outs !! (length outs - 2), outs !! (length outs - 1)]

-- | RASP-L program to extract the calibration value.
-- Assumes inputs are presented with SOS and EOS tokens flanking
-- the real input tokens.
raspCalVals :: [Token] -> [Token]
raspCalVals inputs =
  -- Our output is either the tens digit or the ones digit
  outputTensDigit ? (tensDigit, onesDigit)
  where
    -- We only output the tens digit if the most recent token is EOS.
    outputTensDigit :: BoolSequence
    outputTensDigit = map (== eos) inputs

    -- We retrieve the tens/ones digits based on their indices.
    tensDigit :: Sequence
    tensDigit = maxKQV (indicesOf inputs) tensDigitIdx (==) inputs

    onesDigit :: Sequence
    onesDigit = maxKQV (indicesOf inputs) onesDigitIdx (==) inputs

    -- The tens digit of the calibration value is the first digit (_min_KQV)
    -- that appears after (>) the most recent SOS token.
    tensDigitIdx :: Sequence
    tensDigitIdx = minKQV lastDigitIdx lastSOSIdx (>) lastDigitIdx

    -- The ones digit of the calibration value is the last digit (_max_KQV)
    -- that appears before (<) the most recent EOS token.
    onesDigitIdx :: Sequence
    onesDigitIdx = maxKQV lastDigitIdx lastEOSIdx (<) lastDigitIdx

    -- We use maxKQV to compute a "running maximum" of the indices of the
    -- SOS and EOS tokens.
    lastSOSIdx :: Sequence
    lastSOSIdx = maxKQV inputs (inputs `filledWith` sos) (==) (indicesOf inputs)

    lastEOSIdx :: Sequence
    lastEOSIdx = maxKQV inputs (inputs `filledWith` eos) (==) (indicesOf inputs)

    -- We detect digits by checking if the value is between 1 and 9.
    lastDigitIdx :: Sequence
    lastDigitIdx = maxKQV isDigits (inputs `filledWith` 1) (==) (indicesOf inputs)

    isDigit :: Token -> Bool
    isDigit t = (t > 0) && (t < 10)

    isDigits :: Sequence
    isDigits = tokMap (fromBool . isDigit) inputs

parseInput :: String -> [String]
parseInput = lines

-- | Tokenizes according to the instructions in the first half of the puzzle.
tokenize :: String -> [Token]
tokenize ts = [sos] ++ map charToInt8 ts ++ [eos]
  where
    charToInt8 :: Char -> Token
    charToInt8 c
      | isDigit c = fromIntegral (ord c - ord '0')
      | isLower c = fromIntegral (ord c - ord 'a' + 10)
      | otherwise = error "Invalid character"

-- | Tokenizes according to the instructions in the second half of the puzzle.
tokenize' :: String -> [Token]
tokenize' = tokenize . replace stringDigitMap
  where
    stringDigitMap :: [(String, String)]
    stringDigitMap = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

-- | Helper function to replace spelled-out digits with literal digits.
replace :: [(String, String)] -> String -> String
replace patterns = go
  where
    go "" = ""
    go s@(_ : rest) =
      replaceAtStart s ++ go rest

    -- Swap the first character of a string with a replacement string
    -- if the string prefix matches the paired pattern.
    replaceAtStart :: String -> String
    replaceAtStart str = case filter (\(pat, _) -> pat `isPrefixOf` str) patterns of
      ((_, rep) : _) -> rep
      _ -> [head str]

-- | Adds all calibration values into a single multi-digit number.
sumCalVals :: [CalibrationValue] -> Int
sumCalVals = foldr addUp 0

-- | Adds a calibration value to a number.
addUp :: CalibrationValue -> Int -> Int
addUp [tens, ones] n = 10 * fromIntegral tens + fromIntegral ones + n
addUp cv n = error ("Invalid calibration value or number: " ++ show cv ++ ", " ++ show n)

sos :: Token
sos = -1

eos :: Token
eos = -2

-- These functions are useful for debugging in `ghci`.

showSequence :: Sequence -> String
showSequence = unwords . map showToken

showToken :: Token -> String
showToken token
  | token == sos = "SOS "
  | token == eos = " EOS"
  | otherwise = zfill 4 (fromIntegral token)

zfill :: Int -> Int -> String
zfill width = printf ("%0" ++ show width ++ "d")
