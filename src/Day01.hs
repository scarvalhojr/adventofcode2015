{-# LANGUAGE OverloadedStrings #-}

module Day01
  ( solve
  )
where

import           Data.Text (Text)
import qualified Data.Text as T (uncons)
import           Lib       (Answer (..), DayResult, PartResult)

solve :: Text -> DayResult
solve input = (part1 input, part2 input)

part1 :: Text -> PartResult
part1 = fmap IntAnswer . levelFrom 0

levelFrom :: Int -> Text -> Either Text Int
levelFrom level text = case T.uncons text of
  Just ('(', xs) -> levelFrom (level + 1) xs
  Just (')', xs) -> levelFrom (level - 1) xs
  Nothing        -> Right level
  _              -> Left "Unexpected character in the input"

part2 :: Text -> PartResult
part2 = fmap IntAnswer . position (-1)

position :: Int -> Text -> Either Text Int
position target = count 0 0
  where
    count level pos text
      | level == target  = Right pos
      | otherwise        = case T.uncons text of
        Just ('(', xs) -> count (level + 1) (pos + 1) xs
        Just (')', xs) -> count (level - 1) (pos + 1) xs
        Nothing        -> Left "Not found"
        _              -> Left "Unexpected character in the input"
