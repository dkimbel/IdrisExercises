twice : (a -> a) -> a -> a
twice f x = f (f x)

double : Num a => a -> a
double x = x + x

quadruple : Num a => a -> a
quadruple = twice double

-- As noted by the book, these are type definitions w/o definitions
Shape : Type
rotate : Shape -> Shape

turn_around : Shape -> Shape
turn_around = twice rotate

add : Num a => a -> a -> a
add x y = x + y
