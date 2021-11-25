-- Somewhat non-optmised but still working for simple programs Brainfuck interpreter
module Brainfuck (interpret) where

import Data.Char

type Tape = ([Int], [Int])

zeroes :: [Int]
zeroes = 0 : zeroes

emptyTape :: Tape
emptyTape = (zeroes, zeroes)

execute :: Char -> Tape -> Tape
execute '<' (curr : prev, next) = (prev, curr : next)
execute '>' (prev, curr : next) = (curr : prev, next)
execute '+' (prev, curr : next) = (prev, (curr + 1) : next)
execute '-' (prev, curr : next) = (prev, (curr - 1) : next)
execute c _ = error ("Invalid character " ++ [c])

run :: String -> String -> Tape -> IO (String, Tape)
run "" (']' : _) _ = error "Invalid end of loop"
run _ (']' : prog) (prev, 0 : next) = return (prog, (prev, 0 : next))
run begin (']' : prog) tape = run begin begin tape
run begin ('[' : prog) tape = run prog prog tape >>= \(next_prog, next_tape) -> run begin next_prog next_tape
run begin ('.' : prog) (prev, curr : next) = putChar (chr curr) >> run begin prog (prev, curr : next)
run begin (',' : prog) (prev, curr : next) = getChar >>= \c -> run begin prog (prev, (ord c) : next)
run _ "" tape = putStrLn "" >> putStrLn "End of program" >>  return ("", tape)
run begin (c : prog) tape = run begin prog $ execute c tape

interpret :: String -> IO ()
interpret program = run "" program emptyTape >> return ()
