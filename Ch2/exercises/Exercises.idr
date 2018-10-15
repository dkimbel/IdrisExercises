module Exercises

||| Return whether the given string is a palindrome (case insensitive),
||| and is greater than the given number of characters in length.
palindromeLen : Nat -> String -> Bool
palindromeLen n s = let ls = toLower s in
                        length ls > n && ls == reverse ls

||| Return whether the given string is a palindrome (case insensitive)
export
palindrome : String -> Bool
palindrome s = let ls = toLower s in
                   ls == reverse ls

||| Return the number of words and characters in the input.
export
counts : String -> (Nat, Nat)
counts s = (numWords, numChars)
  where
    numWords = length (words s)
    numChars = length s

top_ten : Ord a => List a -> List a
top_ten l = take 10 (reverse (sort l))

||| Return the number of strings in the given list that are longer than
||| given number of characters.
over_length : Nat -> List String -> Nat
over_length n ss = let lengths = map length ss
                       greaters = filter (>n) lengths in
                       length greaters
