import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_map_vec : (a -> b) -> Vect n a -> Vect n b
my_map_vec f [] = []
my_map_vec f (x :: xs) = f x :: my_map_vec f xs
