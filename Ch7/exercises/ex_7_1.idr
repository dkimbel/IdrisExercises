data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle b h) (Triangle b' h') =
      b == b' && h == h'
  (==) (Rectangle l w) (Rectangle l' w') =
      l == l' && w == w'
  (==) (Circle r) (Circle r') =
      r == r'
  (==) _ _ = False

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l h) = l * h
area (Circle r) = pi * r * r

Ord Shape where
    compare shape shape' = compare (area shape) (area shape')

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
