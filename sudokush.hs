-- sudokush - A shell to solve Sudoku
-- Copyright (C) 2010 Kana Natsuno <kana at whileimautomaton dot net>
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

import Data.Array
import System.Exit
import System.IO
import System.IO.Error




type RowIndex = Int
type ColumnIndex = Int
data Value = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
data Grid = Grid (Array (RowIndex, ColumnIndex) Value)

makeEmptyGrid :: Grid
makeEmptyGrid =
  Grid $ array ((1, 1), (9, 9)) [((r, c), V0) | r <- [1..9], c <- [1..9]]

putGrid :: Grid -> (RowIndex, ColumnIndex) -> Value -> Grid
putGrid (Grid a) (r, c) v = Grid $ a // [((r, c), v)]




data ShellState = ShellState Grid

eval :: ShellState -> String -> IO ShellState
eval s line = evalWords s $ words line

evalWords :: ShellState -> [String] -> IO ShellState
evalWords s [] = return s
evalWords s xs = putStrLn ("Unknown command: `" ++ unwords xs ++ "'") >>
                    return s




repl :: ShellState -> IO ()
repl s = putStr ">>> " >>
         hFlush stdout >>
         getLine `catch` onEof >>=
         eval s >>= \s' ->
         putStr "\n" >>
         repl s'
         where
           onEof e
             | isEOFError e = putStr "\n" >> exitWith ExitSuccess
             | otherwise = ioError e

main = repl $ ShellState $ makeEmptyGrid

-- __END__
-- vim: foldmethod=marker
