{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO (putStrLn, readFile)
import           Day01              (solve)
import           Day02              (solve)
import           Day03              (solve)
import           Day04              (solve)
import           Day05              (solve)
import           Day06              (solve)
import           Day07              (solve)
import           Day08              (solve)
import           Day09              (solve)
import           Day10              (solve)
import           Day11              (solve)
import           Day12              (solve)
import           Day13              (solve)
import           Day14              (solve)
import           Day15              (solve)
import           Day16              (solve)
import           Day17              (solve)
import           Day18              (solve)
import           Day19              (solve)
import           Day20              (solve)
import           Day21              (solve)
import           Day22              (solve)
import           Day23              (solve)
import           Day24              (solve)
import           Day25              (solve)
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
runDay 2 input = Day02.solve input
runDay 3 input = Day03.solve input
runDay 4 input = Day04.solve input
runDay 5 input = Day05.solve input
runDay 6 input = Day06.solve input
runDay 7 input = Day07.solve input
runDay 8 input = Day08.solve input
runDay 9 input = Day09.solve input
runDay 10 input = Day10.solve input
runDay 11 input = Day11.solve input
runDay 12 input = Day12.solve input
runDay 13 input = Day13.solve input
runDay 14 input = Day14.solve input
runDay 15 input = Day15.solve input
runDay 16 input = Day16.solve input
runDay 17 input = Day17.solve input
runDay 18 input = Day18.solve input
runDay 19 input = Day19.solve input
runDay 20 input = Day20.solve input
runDay 21 input = Day21.solve input
runDay 22 input = Day22.solve input
runDay 23 input = Day23.solve input
runDay 24 input = Day24.solve input
runDay 25 input = Day25.solve input
runDay _ _     = (Left "Invalid day", Left "Invalid day")

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
