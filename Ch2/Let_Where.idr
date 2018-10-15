module Main

import Data.Vect

longer : (str : String) -> String -> Nat
longer word1 word2
    = let len1 = length word1
          len2 = length word2 in
          if len1 > len2 then len1 else len2

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x

||| This is a list that I made
myList : List Char
myList = ['a', 'b', 'c']

myStrs : Vect 4 String
myStrs = ["oh", "well", "hi", "there"]

-- Apparently it is not!
isWhereReallyOnlyForFuncs : Num a => a -> a -> a
isWhereReallyOnlyForFuncs x y =
    x + y + z
      where
        z = 15

vecMap : (a -> b) -> Vect n a -> Vect n b
vecMap f [] = []
vecMap f (x :: xs) = f x :: vecMap f xs

vectMap : (a -> b) -> Vect n a -> Vect n b
vectMap f [] = ?vectMap_rhs_1
vectMap f (x :: xs) = ?vectMap_rhs_2

vectMap2 : (a -> b) -> Vect n a -> Vect n b