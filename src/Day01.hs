{-# LANGUAGE OverloadedStrings #-}

module Day01
  ( solve,
  )
where

import           Data.Text (Text)
import qualified Data.Text as T (uncons)
import           Lib       (DayAnswer, PartAnswer, eitherIntToAnswer)

solve :: Text -> DayAnswer
solve input = (part1 input, part2 input)

part1 :: Text -> PartAnswer
part1 = eitherIntToAnswer . level

level :: Text -> Either Text Int
level = count 0
  where
    count c text = case T.uncons text of
      Just ('(', xs) -> count (c + 1) xs
      Just (')', xs) -> count (c - 1) xs
      Nothing        -> Right c
      _              -> Left "Unexpected character in the input"

part2 :: Text -> PartAnswer
part2 = eitherIntToAnswer . position (-1)

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
