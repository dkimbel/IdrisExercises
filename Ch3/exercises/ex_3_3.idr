import Data.Vect

total createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

total transposeMatZip : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatZip [] = createEmpties
transposeMatZip (row :: rows) = let rowsTrans = transposeMatZip rows in
                                    zipWith (::) row rowsTrans

total addRows : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
addRows [] [] = []
addRows (x :: xs) (y :: ys) = x + y :: addRows xs ys

total addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addRows x y :: addMatrix xs ys

total multRow : Num a => (x : Vect m a) -> (y : Vect m a) -> a
multRow x y = sum (zipWith (*) x y)

total multRows : Num a => (x : Vect m a) -> (ys : Vect p (Vect m a)) -> Vect p a
multRows x [] = []
multRows x (y :: ys) = multRow x y :: multRows x ys

total multTransposed : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
multTransposed [] _ = []
multTransposed (x :: xs) ys = multRows x ys :: multTransposed xs ys

total multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multTransposed xs (transposeMatZip ys)
