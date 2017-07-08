module Chapter4_1

import Data.Vect

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x tree@(Node left e right) =
  case compare x e of
    LT => Node (insert x left) e right
    EQ => tree
    GT => Node left e (insert x right)

-- exercise 1
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)


-- exercise 2
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = (treeToList left) ++ (x :: treeToList right)

-- exercise 3
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

%name Expr expr, expr1, expr2

-- exercise 4
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr1 expr2) = (evaluate expr1) + (evaluate expr2)
evaluate (Sub expr1 expr2) = (evaluate expr1) - (evaluate expr2)
evaluate (Mult expr1 expr2) = (evaluate expr1) * (evaluate expr2)

-- exercise 5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (max x y)

-- exercise 6
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive pic@(Triangle base height)) = Just (area pic)
biggestTriangle (Primitive pic) = Nothing
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
