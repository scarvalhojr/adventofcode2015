{-# LANGUAGE OverloadedStrings #-}

module Day03
  ( solve,
  )
where

import           Data.Set  as S (Set, fromList, size, union)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Lib       (DayAnswer, PartAnswer, intToAnswer, invalidInput)

data Move = North | South | East | West

parseMove :: Char -> Maybe Move
parseMove '^' = Just North
parseMove 'v' = Just South
parseMove '>' = Just East
parseMove '<' = Just West
parseMove _   = Nothing

parseInput :: Text -> Maybe [Move]
parseInput = mapM parseMove . T.unpack

data Coord = Coord Int Int
  deriving (Eq, Ord)

move :: Coord -> Move -> Coord
move (Coord x y) North = Coord x (y - 1)
move (Coord x y) South = Coord x (y + 1)
move (Coord x y) East  = Coord (x - 1) y
move (Coord x y) West  = Coord (x + 1) y

solve :: Text -> DayAnswer
solve text = case parseInput text of
  Just moves -> (part1 moves, part2 moves)
  _          -> invalidInput

part1 :: [Move] -> PartAnswer
part1 = intToAnswer . S.size . houses

houses :: [Move] -> S.Set Coord
houses = S.fromList . scanl move (Coord 0 0)

part2 :: [Move] -> PartAnswer
part2 = intToAnswer . S.size . houses2

houses2 :: [Move] -> S.Set Coord
houses2 moves = S.union santa robot
  where
    santa = houses moves1
    robot = houses moves2
    (moves1, moves2) = splitList moves

splitList :: [a] -> ([a], [a])
splitList [] = ([], [])
splitList (x:xs) = (x:ys, zs)
  where (zs, ys) = splitList xs
