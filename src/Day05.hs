{-# LANGUAGE OverloadedStrings #-}

module Day05
  ( solve
  )
where

import           Data.List (elemIndices)
import           Data.Text (Text)
import qualified Data.Text as T (filter, group, isInfixOf, length, lines, tail,
                                 take, unpack, zip)
import           Lib       (Answer (..), DayResult, PartResult)

solve :: Text -> DayResult
solve input = (part1 strings, part2 strings)
  where
    strings = T.lines input

part1 :: [Text] -> PartResult
part1 = Right . IntAnswer . length . filter isNice1

isNice1 :: Text -> Bool
isNice1 str = threeVowels && twiceRepeat && not forbidden
  where
    threeVowels = (==) 3 $ T.length $ T.take 3 $ T.filter isVowel str
    isVowel     = flip elem ("aeiou" :: [Char])
    twiceRepeat = any ((>=2) . T.length) $ T.group str
    forbidden   = any (`T.isInfixOf` str) ["ab", "cd", "pq", "xy"]

part2 :: [Text] -> PartResult
part2 = Right . IntAnswer . length . filter isNice2

isNice2 :: Text -> Bool
isNice2 str = repeatingPair str && singleGapRepeat (T.unpack str)

repeatingPair :: Text -> Bool
repeatingPair str = any repeats allpairs
  where
    allpairs                = T.zip str (T.tail str)
    repeats pair            = nonOverlapRepeat (elemIndices pair allpairs)
    nonOverlapRepeat []     = False
    nonOverlapRepeat (p:ps) = any (> p + 1) ps || nonOverlapRepeat ps

singleGapRepeat :: [Char] -> Bool
singleGapRepeat []         = False
singleGapRepeat [_]        = False
singleGapRepeat [_,_]      = False
singleGapRepeat (a:b:c:xs) = (a == c) || singleGapRepeat (b:c:xs)
