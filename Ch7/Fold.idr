totalLen : List String -> Nat
totalLen xs = foldr (\str, n => length str + n) 0 xs
