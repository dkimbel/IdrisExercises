module Main

import System

guess : (target : Nat) -> IO ()
guess target = do
    putStr "Enter your guess: "
    nstr <- getLine
    case (all isDigit (unpack nstr)) of
         False => do putStrLn "Please enter a valid number."
                     guess target
         True => do let n = cast nstr
                    case compare n target of
                         LT => do putStrLn "Your guess was too low."
                                  guess target
                         EQ => do putStrLn "Correct!"
                                  pure ()
                         GT => do putStrLn "Your guess was too high."
                                  guess target

guessWithCount : (target : Nat) -> (guesses : Nat) -> IO ()
guessWithCount target guesses = do
    putStrLn ("You have guessed " ++ show guesses ++ " time(s).")
    putStr "Enter your guess: "
    nstr <- getLine
    case (all isDigit (unpack nstr)) of
         False => do putStrLn "Please enter a valid number."
                     guessWithCount target guesses -- don't count this against user's total
         True => do let n = cast nstr
                    let totGuesses = guesses + 1
                    case compare n target of
                         LT => do putStrLn "Your guess was too low."
                                  guessWithCount target totGuesses
                         EQ => do putStrLn ("Correct! You got the answer after " ++ show totGuesses ++ " guess(es).")
                                  pure ()
                         GT => do putStrLn "Your guess was too high."
                                  guessWithCount target totGuesses

repl' : String -> (String -> String) -> IO ()
repl' prompt f = do putStr prompt
                    s <- getLine
                    putStr (f s)
                    repl' prompt f

replWith' : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt f = do
    putStr prompt
    s <- getLine
    case f state s of
         Nothing => pure ()
         Just (out, state') => do putStr out
                                  replWith' state' prompt f

main : IO ()
main = do t <- time
          let n = mod t 100
          guessWithCount (cast n) 0
