import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties
--createEmpties = replicate _ []

transposeHelper : (row : Vect n elem) -> (xsTrans : Vect n (Vect m elem)) ->
                  Vect n (Vect (S m) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (row :: rows) = let rowsTrans = transposeMat rows in
                                 transposeHelper row rowsTrans
