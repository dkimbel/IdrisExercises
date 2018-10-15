||| Represents shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

data Biggest = NoTriangle
             | Size Double

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2
%name Biggest biggest, biggest1, biggest2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

testTriangles : Picture
testTriangles = Combine (Translate 5 5 (Primitive (Triangle 40 30)))
                (Combine (Translate 35 5 (Primitive (Triangle 50 50)))
                (Translate 15 25 (Primitive (Triangle 20 20))))

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l h) = l * h
area (Circle r) = pi * r * r

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

comparePossibleTriangles : Biggest -> Biggest -> Biggest
comparePossibleTriangles NoTriangle NoTriangle = NoTriangle
comparePossibleTriangles NoTriangle (Size x) = Size x
comparePossibleTriangles (Size x) NoTriangle = Size x
comparePossibleTriangles (Size x) (Size y) = if (x > y) then Size x else Size y

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive shape) = case shape of
                                        (Triangle _ _) => Size (area shape)
                                        _ => NoTriangle
biggestTriangle (Combine pic pic1) = comparePossibleTriangles (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
