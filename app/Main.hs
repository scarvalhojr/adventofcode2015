{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text          (Text)
import qualified Data.Text          as T (pack)
import qualified Data.Text.IO       as TIO (readFile)
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
import           Fmt                (build, fmt, fmtLn, nameF, (+|), (|+))
import           Lib                (Answer (..), DayResult, PartResult)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)
import           TextShow           (showb, TextShow)

main :: IO ()
main = do
  fmtLn "Advent of Code 2015"
  args <- getArgs
  case args of
    [day, filename] -> run day filename
    _               -> usage

run :: String -> FilePath -> IO ()
run day filename = do
  case readMaybe day :: Maybe Int of
    Just number | number >= 1 && number <= 25 -> do
      fmtLn $ "Day " +| number |+ ""
      input <- TIO.readFile filename
      report $ solver number input
      where
        solver n = solvers !! (n - 1)
        solvers  =
          [ Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve
          , Day06.solve, Day07.solve, Day08.solve, Day09.solve, Day10.solve
          , Day11.solve, Day12.solve, Day13.solve, Day14.solve, Day15.solve
          , Day16.solve, Day17.solve, Day18.solve, Day19.solve, Day20.solve
          , Day21.solve, Day22.solve, Day23.solve, Day24.solve, Day25.solve
          ]
    _ -> do
      fmtLn "Error: invalid day"
      usage

report :: DayResult -> IO ()
report (answer1, answer2) = do
  fmt $ nameF "Part 1" (format answer1)
  fmt $ nameF "Part 2" (format answer2)
  where
    format (Left error)   = "Error: " +| error |+ ""
    format (Right answer) = showb answer

usage :: IO ()
usage = do
  fmtLn "Usage: aoc <day number> <input file name>"
