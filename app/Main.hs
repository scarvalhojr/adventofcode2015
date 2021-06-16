{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO (putStrLn, readFile)
import           Day01              (solve)
import           Fmt                (fmt, (+|), (|+))
import           Lib                (DayAnswer, PartAnswer)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

main :: IO ()
main = do
  putStrLn "Advent of Code 2015"
  args <- getArgs
  case args of
    [day, filename] -> run day filename
    _               -> usage

run :: String -> FilePath -> IO ()
run day filename = do
  case readMaybe day :: Maybe Int of
    Just number | number >= 1 && number <= 25 -> do
      putStrLn $ "Day " ++ show number
      input <- TIO.readFile filename
      report $ runDay number input
    _ -> do
      putStrLn "Error: invalid day"
      usage

runDay :: Int -> Text -> DayAnswer
runDay 1 input = Day01.solve input
runDay _ _     = (Left "Not implemented", Left "Not implemented")

report :: DayAnswer -> IO ()
report (answer1, answer2) =
  fmt $
    "Part 1: " +| formatAnswer answer1 |+
    "\nPart 2: " +| formatAnswer answer2 |+ "\n"

formatAnswer :: PartAnswer -> Text
formatAnswer (Left error)   = "Error: " +| error |+ ""
formatAnswer (Right answer) = answer

usage :: IO ()
usage = do
  putStrLn "Usage: aoc <day number> <input file name>"
