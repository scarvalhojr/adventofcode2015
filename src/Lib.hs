{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Answer(..)
  , DayResult
  , PartResult
  , invalidInput
  , maybeToResult
  )
where

import           Data.Text (Text)
import           TextShow (fromText, showb, TextShow)

data Answer
  = IntAnswer Int
  | TextAnswer Text

instance TextShow Answer where
  showb (IntAnswer val)    = showb val
  showb (TextAnswer val)   = fromText val

type PartResult = Either Text Answer
type DayResult = (PartResult, PartResult)

invalidInput :: DayResult
invalidInput = (invalid, invalid)
  where
    invalid = Left "Invalid input"

maybeToResult :: Maybe Answer -> PartResult
maybeToResult (Just answer) = Right answer
maybeToResult Nothing       = Left "Not found"
