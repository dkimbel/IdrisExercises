||| Represents shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l h) = l * h
area (Circle r) = pi * r * r
