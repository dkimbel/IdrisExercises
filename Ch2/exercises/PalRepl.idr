module Main

import Exercises

checkPalindrome : String -> String
checkPalindrome s = show (palindrome s) ++ "\n"

main : IO ()
main = repl "Enter a string: " checkPalindrome
