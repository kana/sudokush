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

import System.Exit
import System.IO
import System.IO.Error




data ShellState = ShellState

eval :: ShellState -> String -> IO ShellState
eval s line = evalWords s $ words line

evalWords :: ShellState -> [String] -> IO ShellState
evalWords s [] = return s
evalWords s xs = putStrLn ("Unknown command: `" ++ unwords xs ++ "'") >>
                    return s




repl :: ShellState -> IO ()
repl s = putStr ">>> " >>
         hFlush stdout >>
         getLine `catch` onEof >>= eval s >>= \x -> (
           putStrLn "" >>
           repl x
         )
         where
           onEof e = if isEOFError e
                       then putStr "\n" >> exitWith ExitSuccess
                       else ioError e

main = repl ShellState

-- __END__
-- vim: foldmethod=marker
