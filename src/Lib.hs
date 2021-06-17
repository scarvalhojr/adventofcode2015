module Lib
  ( PartAnswer,
    DayAnswer,
    eitherIntToAnswer,
    intToAnswer,
    invalidInput,
  )
where

import           Data.Text (Text)
import qualified Data.Text as T (pack)

type PartAnswer = Either Text Text

type DayAnswer = (PartAnswer, PartAnswer)

invalidInput :: DayAnswer
invalidInput = (invalid, invalid)
  where
    invalid = Left (T.pack "Invalid input")

intToAnswer :: Int -> PartAnswer
intToAnswer = Right . T.pack . show

eitherIntToAnswer :: Either Text Int -> PartAnswer
eitherIntToAnswer = fmap (T.pack . show)
