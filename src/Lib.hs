module Lib
  ( Answer(..)
  , DayResult
  , PartResult
  , invalidInput
  )
where

import           Data.Text (Text)
import qualified Data.Text as T (pack)

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
    invalid = Left (T.pack "Invalid input")
