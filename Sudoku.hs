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
  deriving (Eq)
type IndexedCell = (CellIndex, Cell)

data Puzzle = Puzzle {
    grid :: Grid
  }
  deriving (Eq)
type Grid = Map CellIndex Cell

data SolvingTechnique =
  RemovingCandidates
  | HiddenSingle
  | NakedSingle
  -- TODO: Implement more solving techniques.
  deriving (Eq)




allCellIndices :: [CellIndex]
allCellIndices = [(r, c) | r <- [1..9], c <- [1..9]]

allBoxIndexSets :: [[CellIndex]]
allBoxIndexSets = [[(r, c) | r <- [((br - 1) * 3 + 1)..(br * 3)],
                             c <- [((bc - 1) * 3 + 1)..(bc * 3)]]
                   | br <- [1..3], bc <- [1..3]]

allColumnIndexSets :: [[CellIndex]]
allColumnIndexSets = [[(r, c) | r <- [1..9]] | c <- [1..9]]

allRowIndexSets :: [[CellIndex]]
allRowIndexSets = [[(r, c) | c <- [1..9]] | r <- [1..9]]

empty :: Puzzle
empty = Puzzle $ Map.fromList [(i, (Cell 0 False [1..9]))
                               | i <- allCellIndices]

fromList :: [(CellIndex, Digit)] -> Puzzle
fromList ids = solve RemovingCandidates $ Puzzle $ Map.union customSet emptySet
  where
    emptySet = grid empty
    customSet = Map.fromList $ map wrap ids
    wrap (i, d) = (i, Cell {solution = d,
                            initialp = True,
                            candidates = if d == 0 then [1..9] else [d]})

toList :: Puzzle -> [(CellIndex, Digit)]
toList (Puzzle g) = map unwrap $ Map.toList g
  where
    unwrap (i, cell) = (i, solution cell)

parse :: String -> Puzzle
parse s = fromList $ concat $ map wrap $ zip rs dss
  where
    wrap (r, ds) = zipWith (\d c -> ((r, c), d)) ds cs
    rs = [1..9]
    cs = [1..9]
    dss = map (map toInt . map head . words) rows
    rows = lines s
    toInt c
      | c `elem` ['1'..'9'] = read [c]
      | otherwise = 0

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
solve method
  | method == RemovingCandidates = solveByRemovingCandidates
  | method == HiddenSingle = zap solveByHiddenSingle
  | method == NakedSingle = zap solveByNakedSingle
  | otherwise = undefined
  where
    zap s p = if p == p' 
                then p
                else zap s p'
      where
        p' = solveByRemovingCandidates $ s p

settle :: Grid -> CellIndex -> Digit -> Grid
settle g i d = Map.update toSettledCell i g
  where
    toSettledCell cell = Just cell {solution = d, candidates = [d]}


solveByRemovingCandidates :: Puzzle -> Puzzle
solveByRemovingCandidates (Puzzle g0)
  = Puzzle $ foldr zapCell g0 allCellIndices
  where
    zapCell :: CellIndex -> Grid -> Grid
    zapCell i@(r, c) g
      | d /= 0 = cleanBox i d $ cleanColumn c d $ cleanRow r d g
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

cleanBox :: CellIndex -> Digit -> Grid -> Grid
cleanBox i d g = cleanCells (indicesOfBox i) d g

cleanColumn :: ColumnIndex -> Digit -> Grid -> Grid
cleanColumn c d g = cleanCells [(r, c) | r <- [1..9]] d g

cleanRow :: RowIndex -> Digit -> Grid -> Grid
cleanRow r d g = cleanCells [(r, c) | c <- [1..9]] d g

indicesOfBox :: CellIndex -> [CellIndex]
indicesOfBox (r0, c0) = [(r, c) | r <- rs, c <- cs]
  where
    rh = (r0 - 1) `div` 3
    ch = (c0 - 1) `div` 3
    rs = [(rh * 3 + 1)..((rh + 1) * 3)]
    cs = [(ch * 3 + 1)..((ch + 1) * 3)]


solveByHiddenSingle :: Puzzle -> Puzzle
solveByHiddenSingle (Puzzle g0)
  = Puzzle $
    zapEachBox $
    zapEachColumn $
    zapEachRow g0
  where
    zapEachBox g = foldr zapHouse g allBoxIndexSets
    zapEachColumn g = foldr zapHouse g allColumnIndexSets
    zapEachRow g = foldr zapHouse g allRowIndexSets
    zapHouse iset g = foldr (zap iset) g [1..9]
    zap iset d g
      | found = settle g (fst ic) d
      | otherwise = g
      where
        found = and [not $ null ics, null $ tail ics]
        ic = head ics
        ics = filter withCandidate $ zip iset (map (g Map.!) iset)
        withCandidate (_, c) = d `elem` candidates c


solveByNakedSingle :: Puzzle -> Puzzle
solveByNakedSingle (Puzzle g0)
  = Puzzle $ foldr zap g0 allCellIndices
  where
    zap i g
      | isNaked = settle g i (head cs)
      | otherwise = g
      where
        cs = candidates $ g Map.! i
        isNaked = and [not $ null cs, null $ tail cs]




-- __END__
-- vim: foldmethod=marker
