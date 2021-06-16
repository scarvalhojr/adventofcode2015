module Lib
  ( PartAnswer,
    DayAnswer,
  )
where

import           Data.Text (Text)

type PartAnswer = Either Text Text

type DayAnswer = (PartAnswer, PartAnswer)
