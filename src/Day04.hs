{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( solve
  )
where

import           Crypto.Hash          (MD5 (..), hashFinalize, hashInitWith,
                                       hashUpdate)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.UTF8 as BSU (fromString)
import           Data.List            (find)
import           Data.Text            (Text, strip)
import           Data.Text.Encoding   as TSE (encodeUtf8)
import           Lib                  (Answer (..), DayResult, PartResult)

solve :: Text -> DayResult
solve input = (part1 secret, part2 secret)
  where
    secret = encodeUtf8 $ strip input

part1 :: ByteString -> PartResult
part1 = Right . IntAnswer . mineCoin 5

mineCoin :: Int -> ByteString -> Int
mineCoin len secret = head $ filter isCoin [1..]
  where
    init   = hashUpdate (hashInitWith MD5) secret
    hash   = hashFinalize . hashUpdate init . BSU.fromString . show
    isCoin = all ('0' ==) . take len . show . hash

part2 :: ByteString -> PartResult
part2 = Right . IntAnswer . mineCoin 6
