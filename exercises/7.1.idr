module Chapter7_1

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

-- exercise 1
Eq Shape where
  (==) (Triangle base h) (Triangle base' h') = base == base' && h == h'
  (==) (Rectangle len h) (Rectangle len' h') = len == len' && h == h'
  (==) (Circle rad) (Circle rad') = rad == rad'
  (==) _ _ = False

-- exercise 2
Ord Shape where
  compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
