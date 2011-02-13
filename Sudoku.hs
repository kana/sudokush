-- sudokush - A shell to solve Sudoku
-- Copyright (C) 2011 Kana Natsuno <kana at whileimautomaton dot net>
-- License: So-called MIT/X license  {{{
--     Permission is hereby granted, free of charge, to any person obtaining
--     a copy of this software and associated documentation files (the
--     "Software"), to deal in the Software without restriction, including
--     without limitation the rights to use, copy, modify, merge, publish,
--     distribute, sublicense, and/or sell copies of the Software, and to
--     permit persons to whom the Software is furnished to do so, subject to
--     the following conditions:
--
--     The above copyright notice and this permission notice shall be included
--     in all copies or substantial portions of the Software.
--
--     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
--     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
--     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- }}}

module Sudoku (
    Cell,
    ColumnIndex,
    Digit,
    IndexedCell,
    Puzzle,
    RowIndex,
    SolvingTechnique(..),
    empty,
    fromList,
    parse,
    pp,
    solve,
    toList,
  ) where

import Data.Map (Map)
import qualified Data.Map as Map




type Digit = Int

type RowIndex = Int
type ColumnIndex = Int
type CellIndex = (RowIndex, ColumnIndex)

data Cell = Cell {
    solution :: Digit,
    initialp :: Bool,
    candidates :: [Digit]
  }
type IndexedCell = (CellIndex, Cell)

data Puzzle = Puzzle {
    grid :: Grid
  }
type Grid = Map CellIndex Cell

data SolvingTechnique =
  RemovingCandidates
  | NakedSingle
  -- TODO: Implement more solving techniques.




empty :: Puzzle
empty = Puzzle $ Map.fromList [((r, c), (Cell 0 False [1..9]))
                               | r <- [1..9], c <- [1..9]]

fromList :: [IndexedCell] -> Puzzle
fromList ics = Puzzle $ Map.union customSet emptySet
               where
                 emptySet = grid empty
                 customSet = Map.fromList ics

toList :: Puzzle -> [IndexedCell]
toList (Puzzle g) = Map.toList g

parse :: String -> Puzzle
parse = undefined

pp :: Puzzle -> String
pp (Puzzle g) = unlines $ concat $ (slice prows [1..3]
                                    ++ border
                                    ++ slice prows [4..6]
                                    ++ border
                                    ++ slice prows [7..9])
  where
    rows = [[g Map.! (r, c) | c <- [1..9]] | r <- [1..9]]
    prows = map ppRow rows
    border = [[replicate (length $ head $ head prows) '-']]
    slice xs is = map (\i -> xs !! (i - 1)) is

ppRow :: [Cell] -> [String]
ppRow cs = join " | " $ [join " " $ slice pcs [1..3],
                         join " " $ slice pcs [4..6],
                         join " " $ slice pcs [7..9]]
  where
    pcs = map ppCell cs
    slice xss is = map (\i -> xss !! (i - 1)) is
    unzipN [] = []
    unzipN xss
      | any null xss = []
      | otherwise = heads : unzipN tails
        where
          heads = map head xss
          tails = map tail xss
    join sep xss = go $ unzipN xss
      where
        go [] = []
        go (xs:xss') = join' sep xs : go xss'
    join' sep xs = foldr1 (\x1 x2 -> x1 ++ sep ++ x2) xs

ppCell :: Cell -> [String]
ppCell Cell {solution = s, candidates = cs}
  | s /= 0 = ["###", "#" ++ show s ++ "#", "###"]
  | otherwise = [slice pcs [1..3], slice pcs [4..6], slice pcs [7..9]]
    where
      ppCandidates c
        | c `elem` cs = show c
        | otherwise = "."
      pcs = map ppCandidates [1..9]
      slice xs is = concat $ map (\i -> xs !! (i - 1)) is




solve :: SolvingTechnique -> Puzzle -> Puzzle
solve RemovingCandidates = solveByRemovingCandidates
solve _ = undefined

solveByRemovingCandidates :: Puzzle -> Puzzle
solveByRemovingCandidates (Puzzle g0) = Puzzle $ foldr zapCell g0 is
  where
    is :: [CellIndex]
    is = [(r, c) | r <- [1..9], c <- [1..9]]
    zapCell :: CellIndex -> Grid -> Grid
    zapCell i@(r, c) g
      | d /= 0 = cleanRow r d $ cleanColumn c d $ cleanHouse i d g
      | otherwise = g
      where
        cell = g Map.! (r, c)
        d = solution cell

cleanCells :: [CellIndex] -> Digit -> Grid -> Grid
cleanCells is d g0 = foldr clean g0 is
  where
    clean i g = Map.update removeCandidates i g
    removeCandidates cell =
      Just cell {candidates = filter (d /=) (candidates cell)}

cleanColumn :: ColumnIndex -> Digit -> Grid -> Grid
cleanColumn c d g = cleanCells [(r, c) | r <- [1..9]] d g

cleanHouse :: CellIndex -> Digit -> Grid -> Grid
cleanHouse i d g = cleanCells (indicesOfHouse i) d g

cleanRow :: RowIndex -> Digit -> Grid -> Grid
cleanRow r d g = cleanCells [(r, c) | c <- [1..9]] d g

indicesOfHouse :: CellIndex -> [CellIndex]
indicesOfHouse i = []  -- TODO: Implement




-- __END__
-- vim: foldmethod=marker
