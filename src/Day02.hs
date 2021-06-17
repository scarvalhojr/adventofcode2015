{-# LANGUAGE OverloadedStrings #-}

module Day02
  ( solve
  )
where

import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text as T (lines, split, unpack)
import           Lib       (Answer (..), DayResult, PartResult, invalidInput)
import           Text.Read (readMaybe)

data Box = Box Int Int Int

solve :: Text -> DayResult
solve text =
  case parseInput text of
    Just boxes -> (part1 boxes, part2 boxes)
    _          -> invalidInput

parseInput :: Text -> Maybe [Box]
parseInput = mapM parseBox . T.lines

parseBox :: Text -> Maybe Box
parseBox text =
  case values :: Maybe [Int] of
    Just [l, w, h] -> Just (Box l w h)
    _              -> Nothing
  where
    numbers = T.split (== 'x') text
    values  = mapM (readMaybe . T.unpack) numbers

part1 :: [Box] -> PartResult
part1 = Right . IntAnswer . sum . map paper

paper :: Box -> Int
paper (Box l w h) = 2 * lw + 2 * lh + 2 * wh + slack
  where
    lw = l * w
    lh = l * h
    wh = w * h
    slack = min lw (min lh wh)

part2 :: [Box] -> PartResult
part2 = Right . IntAnswer . sum . map ribbon

ribbon :: Box -> Int
ribbon (Box l w h) = perim + vol
  where
    sides = take 2 $ sort [l, w, h]
    perim = 2 * sum sides
    vol = l * w * h
