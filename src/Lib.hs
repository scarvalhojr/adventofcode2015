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

data Answer
  = IntAnswer Int
  | StringAnswer String

instance Show Answer where
    show (IntAnswer val)    = show val
    show (StringAnswer val) = val

type PartResult = Either Text Answer
type DayResult = (PartResult, PartResult)

invalidInput :: DayResult
invalidInput = (invalid, invalid)
  where
    invalid = Left "Invalid input"

maybeToResult :: Maybe Answer -> PartResult
maybeToResult (Just answer) = Right answer
maybeToResult Nothing       = Left "Not found"
