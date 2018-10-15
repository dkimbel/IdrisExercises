module Main

import Exercises

showCounts : String -> String
showCounts s = show (counts s) ++ "\n"

main : IO ()
main = repl "Enter a string: " showCounts
